import { Body, Controller, Post, Res } from '@nestjs/common';
import type { Response } from 'express';
import type { Plan } from '@planner/shared';
import { SolverService } from './solver.service';
import { SolvePlanDto } from './dto/solve-plan.dto';
import type { SolutionDto } from './dto/solution.dto';

/**
 * HTTP controller responsible for solver operations.
 */
@Controller('solver')
export class SolverController {
  constructor(private readonly solverService: SolverService) {}

  @Post('solve')
  async solve(@Body() body: SolvePlanDto, @Res({ passthrough: true }) res: Response): Promise<SolutionDto> {
    const { timeout, ...planPayload } = body;
    const plan = planPayload as unknown as Plan;
    const result = await this.solverService.solvePlan(plan, { timeout });

    res.status(this.resolveStatusCode(result));
    return result;
  }

  private resolveStatusCode(result: SolutionDto): number {
    if (result.success) {
      return 200;
    }

    switch (result.error?.code) {
      case 'INVALID_PLAN':
        return 400;
      case 'SOLVER_TIMEOUT':
        return 408;
      case 'PROLOG_NOT_FOUND':
      case 'PROLOG_ERROR':
        return 500;
      default:
        return 200;
    }
  }
}
