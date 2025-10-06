import { BadRequestException, Injectable, Logger } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { ExportService } from '../export/export.service';
import { toProlog } from '@planner/serializer';
import type { Plan } from '@planner/shared';
import type { ExportPlanDto } from '../export/dto/export-plan.dto';
import {
  DeskPlacementDto,
  SolutionDto,
  SolverErrorDto,
  SolverSolutionPayload,
} from './dto/solution.dto';
import { spawn } from 'node:child_process';
import * as path from 'node:path';
import * as os from 'node:os';
import {
  access,
  constants as fsConstants,
  copyFile,
  mkdtemp,
  mkdir,
  readFile,
  readdir,
  rm,
  writeFile,
} from 'node:fs/promises';
import { existsSync } from 'node:fs';

type NormalizedDesk = DeskPlacementDto;

interface SolverOptions {
  timeout?: number;
}

interface PrologRunResult {
  exitCode: number | null;
  stdout: string;
  stderr: string;
  timedOut: boolean;
}

@Injectable()
export class SolverService {
  private readonly logger = new Logger(SolverService.name);

  constructor(
    private readonly configService: ConfigService,
    private readonly exportService: ExportService,
  ) {}

  /**
   * Runs Prolog solver for provided plan and returns solution description.
   */
  async solvePlan(plan: Plan, options: SolverOptions = {}): Promise<SolutionDto> {
    if (!plan.task) {
      return {
        success: false,
        error: {
          code: 'INVALID_PLAN',
          message:
            'В задаче должно быть указано количество рабочих мест и их размеры.',
        },
      };
    }

    const startedAt = Date.now();

    let prologProgram: string;
    try {
      // Reuse export service validation to ensure geometry correctness
      this.exportService.validatePlanStructure(plan as unknown as ExportPlanDto);
      prologProgram = toProlog(plan);
    } catch (error) {
      if (error instanceof BadRequestException) {
        return {
          success: false,
          error: {
            code: 'INVALID_PLAN',
            message: error.message,
          },
        };
      }
      this.logger.error('Failed to prepare plan for solver', error instanceof Error ? error.stack : undefined);
      return {
        success: false,
        error: {
          code: 'PROLOG_ERROR',
          message: 'Не удалось подготовить план для решателя.',
        },
      };
    }

    const timeoutSeconds = this.resolveTimeout(options.timeout);
    const prologPath = this.resolvePrologExecutable();

    let workspace: string | undefined;
    try {
      workspace = await this.createWorkspace();
      const planFile = path.join(workspace, 'plan.pl');
      await writeFile(planFile, prologProgram, 'utf8');

      const runResult = await this.runPrologSolver(workspace, prologPath, timeoutSeconds);
      if (runResult.timedOut) {
        this.logger.warn(`Solver timed out after ${timeoutSeconds}s`);
        return {
          success: false,
          error: {
            code: 'SOLVER_TIMEOUT',
            message: `Решение превысило лимит в ${timeoutSeconds} секунд. Попробуйте упростить план или увеличить таймаут.`,
          },
          metadata: {
            executionTime: Date.now() - startedAt,
            placedDesks: 0,
            requestedDesks: plan.task.count,
          },
        };
      }

      if (runResult.exitCode !== 0) {
        this.logger.error(`Solver exited with code ${runResult.exitCode}: ${runResult.stderr}`);
        return {
          success: false,
          error: {
            code: 'PROLOG_ERROR',
            message: 'Решатель завершился с ошибкой. Подробности смотрите в логах.',
          },
          metadata: {
            executionTime: Date.now() - startedAt,
            placedDesks: 0,
            requestedDesks: plan.task.count,
          },
        };
      }

      const solutionPath = path.join(workspace, 'solution.json');
      const solutionContent = await this.safeReadFile(solutionPath);
      if (!solutionContent) {
        this.logger.error('Solution file was not produced by solver');
        return {
          success: false,
          error: {
            code: 'PROLOG_ERROR',
            message: 'Решатель не вернул результат. Попробуйте повторить попытку.',
          },
          metadata: {
            executionTime: Date.now() - startedAt,
            placedDesks: 0,
            requestedDesks: plan.task.count,
          },
        };
      }

      let parsed: unknown;
      try {
        parsed = JSON.parse(solutionContent);
      } catch (error) {
        this.logger.error('Failed to parse solver output as JSON', error instanceof Error ? error.stack : undefined);
        return {
          success: false,
          error: {
            code: 'PROLOG_ERROR',
            message: 'Не удалось разобрать ответ решателя.',
          },
          metadata: {
            executionTime: Date.now() - startedAt,
            placedDesks: 0,
            requestedDesks: plan.task.count,
          },
        };
      }

      const normalized = this.normalizeSolution(parsed, plan.task.count);
      if (!normalized.success) {
        return {
          success: false,
          error: normalized.error,
          metadata: {
            executionTime: Date.now() - startedAt,
            placedDesks: normalized.placed,
            requestedDesks: plan.task.count,
          },
        };
      }

      this.logger.log(
        `Solver completed in ${Date.now() - startedAt}ms. Placed ${normalized.solution.desks.length}/${plan.task.count} desks`,
      );

      return {
        success: true,
        solution: normalized.solution,
        metadata: {
          executionTime: Date.now() - startedAt,
          placedDesks: normalized.solution.desks.length,
          requestedDesks: plan.task.count,
        },
      };
    } catch (error) {
      if (this.isPrologNotFound(error)) {
        return {
          success: false,
          error: {
            code: 'PROLOG_NOT_FOUND',
            message: 'Решатель Prolog не найден. Убедитесь, что SWI-Prolog установлен.',
          },
          metadata: {
            executionTime: Date.now() - startedAt,
            placedDesks: 0,
            requestedDesks: plan.task.count,
          },
        };
      }

      this.logger.error('Solver execution failed', error instanceof Error ? error.stack : undefined);
      return {
        success: false,
        error: {
          code: 'PROLOG_ERROR',
          message: 'Произошла непредвиденная ошибка при выполнении решателя.',
        },
        metadata: {
          executionTime: Date.now() - startedAt,
          placedDesks: 0,
          requestedDesks: plan.task.count,
        },
      };
    } finally {
      if (workspace) {
        await this.cleanupWorkspace(workspace);
      }
    }
  }

  private resolveTimeout(requestTimeout?: number): number {
    const defaultTimeout = Number(this.configService.get('SOLVER_TIMEOUT')) || 30;
    const timeout = requestTimeout ?? defaultTimeout;
    return Math.min(Math.max(timeout, 5), 300);
  }

  private resolvePrologExecutable(): string {
    return this.configService.get<string>('PROLOG_PATH') || 'swipl';
  }

  private async createWorkspace(): Promise<string> {
    const configuredDir = this.configService.get<string>('SOLVER_TEMP_DIR');
    const baseDir = configuredDir || path.join(os.tmpdir(), 'planner-solver');
    await mkdir(baseDir, { recursive: true });
    const workspace = await mkdtemp(path.join(baseDir, 'run-'));
    await this.copyPrologModules(workspace);
    return workspace;
  }

  private async cleanupWorkspace(workspace: string): Promise<void> {
    try {
      await rm(workspace, { recursive: true, force: true });
    } catch (error) {
      this.logger.warn(`Failed to cleanup workspace ${workspace}: ${(error as Error).message}`);
    }
  }

  private async copyPrologModules(targetDir: string): Promise<void> {
    const modulesPath = this.resolveModulesPath();
    await access(modulesPath, fsConstants.R_OK);
    const entries = await readdir(modulesPath, { withFileTypes: true });
    await Promise.all(
      entries
        .filter((entry) => entry.isFile() && entry.name.endsWith('.pl'))
        .map((entry) =>
          copyFile(path.join(modulesPath, entry.name), path.join(targetDir, entry.name)),
        ),
    );
  }

  private resolveModulesPath(): string {
    const configured = this.configService.get<string>('PROLOG_MODULES_PATH');
    if (configured) {
      return configured;
    }

    const candidates = [
      path.resolve(process.cwd(), 'prolog'),
      path.resolve(process.cwd(), '../prolog'),
      path.resolve(__dirname, '../../../../prolog'),
      path.resolve(__dirname, '../../../../../prolog'),
    ];

    for (const candidate of candidates) {
      if (existsSync(candidate)) {
        return candidate;
      }
    }

    return path.resolve(process.cwd(), 'prolog');
  }

  private async runPrologSolver(
    workspace: string,
    executable: string,
    timeoutSeconds: number,
  ): Promise<PrologRunResult> {
    const goal = [
      "load_files(['rects','zones','tiles','layout_spiral','orient_spiral','plan_loader_spiral','plan']),",
      "plan_loader_spiral:load_plan('plan.pl'),",
      'plan_loader_spiral:solve_plan_spiral_oriented(Rects, Oris, Grid),',
      "plan_loader_spiral:solve_plan_spiral_oriented_to_file('solution.json', json),",
      'halt.',
    ].join(' ');

    return new Promise<PrologRunResult>((resolve, reject) => {
      const child = spawn(executable, ['-q', '-g', goal, '-t', 'halt(1)'], {
        cwd: workspace,
      });

      let stdout = '';
      let stderr = '';
      let timedOut = false;

      const timeout = setTimeout(() => {
        timedOut = true;
        child.kill('SIGKILL');
      }, timeoutSeconds * 1000);

      child.stdout?.on('data', (chunk: Buffer) => {
        stdout += chunk.toString();
      });

      child.stderr?.on('data', (chunk: Buffer) => {
        stderr += chunk.toString();
      });

      child.on('error', (error) => {
        clearTimeout(timeout);
        reject(error);
      });

      child.on('close', (code) => {
        clearTimeout(timeout);
        resolve({ exitCode: code, stdout, stderr, timedOut });
      });
    });
  }

  private async safeReadFile(filePath: string): Promise<string | null> {
    try {
      await access(filePath, fsConstants.R_OK);
      return await readFile(filePath, 'utf8');
    } catch {
      return null;
    }
  }

  private normalizeSolution(parsed: unknown, requested: number): {
    success: boolean;
    solution: SolverSolutionPayload;
    error?: SolverErrorDto;
    placed: number;
  } {
    if (typeof parsed !== 'object' || parsed === null) {
      return {
        success: false,
        solution: { desks: [] },
        error: {
          code: 'PROLOG_ERROR',
          message: 'Ответ решателя имеет неверный формат.',
        },
        placed: 0,
      };
    }

    const payload = parsed as { grid?: unknown; desks?: unknown };
    const desks = Array.isArray(payload.desks) ? payload.desks : [];

    const normalizedDesks: NormalizedDesk[] = desks
      .map((desk) => {
        if (typeof desk !== 'object' || desk === null) {
          return null;
        }
        const item = desk as Record<string, unknown>;
        const rect = item.rect as Record<string, unknown> | undefined;
        if (!rect) {
          return null;
        }
        const x = Number(rect.x);
        const y = Number(rect.y);
        const w = Number(rect.w);
        const h = Number(rect.h);
        if ([x, y, w, h].some((value) => Number.isNaN(value))) {
          return null;
        }
        const orientationRaw = Number(item.orientation);
        const orientation = Number.isFinite(orientationRaw)
          ? ((Math.round(orientationRaw) % 4) + 4) % 4
          : undefined;
        const baseSizeRaw = item.base_size as Record<string, unknown> | undefined;
        const baseSize = baseSizeRaw
          ? {
              w: Number(baseSizeRaw.w) || w,
              h: Number(baseSizeRaw.h) || h,
            }
          : undefined;
        const id = typeof item.id === 'string' && item.id.length > 0 ? item.id : undefined;
        const type = typeof item.type === 'string' ? item.type : 'desk';
        const normalized: NormalizedDesk = {
          id: id ?? `desk-${Math.random().toString(36).slice(2, 8)}`,
          type,
          rect: { x, y, w, h },
          orientation,
          base_size: baseSize,
        };
        return normalized;
      })
      .filter((desk): desk is NormalizedDesk => Boolean(desk));

    if (normalizedDesks.length === 0 || normalizedDesks.length < requested) {
      return {
        success: false,
        solution: { desks: normalizedDesks },
        error: {
          code: 'NO_SOLUTION_FOUND',
          message:
            'Не удалось разместить все рабочие места. Попробуйте изменить параметры задачи.',
          details: {
            requested,
            placed: normalizedDesks.length,
          },
        },
        placed: normalizedDesks.length,
      };
    }

    return {
      success: true,
      solution: {
        grid: typeof payload.grid === 'number' ? payload.grid : undefined,
        desks: normalizedDesks,
      },
      placed: normalizedDesks.length,
    };
  }

  private isPrologNotFound(error: unknown): boolean {
    return Boolean((error as NodeJS.ErrnoException)?.code === 'ENOENT');
  }
}
