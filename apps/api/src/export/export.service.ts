import { BadRequestException, Injectable, Logger } from '@nestjs/common';
import { toProlog } from '@planner/serializer';
import type { Plan } from '@planner/shared';
import { ExportPlanDto } from './dto/export-plan.dto';

/**
 * Handles business logic for exporting planner data.
 */
@Injectable()
export class ExportService {
  private readonly logger = new Logger(ExportService.name);

  /**
   * Converts a validated plan into Prolog representation.
   * @param plan plan payload.
   * @returns serialized Prolog program.
   */
  exportToPrologFormat(plan: ExportPlanDto): string {
    this.logger.debug(
      `Exporting plan: room=${plan.room.W}x${plan.room.H}, objects=${plan.objects.length}`,
    );

    this.validatePlanStructure(plan);

    try {
      const prologCode = toProlog(plan as unknown as Plan);
      this.logger.log('Plan successfully exported to Prolog');
      return prologCode;
    } catch (error) {
      this.logger.error('Failed to serialize plan', error instanceof Error ? error.stack : undefined);
      throw error;
    }
  }

  /**
   * Performs structural validation of plan data beyond DTO validation.
   */
  validatePlanStructure(plan: ExportPlanDto): void {
    const outsideObjects = plan.objects.filter((object) => {
      const { X, Y, W, H } = object.rect;
      return X < 0 || Y < 0 || X + W > plan.room.W || Y + H > plan.room.H;
    });

    if (outsideObjects.length > 0) {
      const ids = outsideObjects.map((obj) => obj.id).join(', ');
      throw new BadRequestException(`Objects outside room boundaries: ${ids}`);
    }

    const seen = new Set<string>();
    const duplicates = new Set<string>();
    for (const object of plan.objects) {
      if (seen.has(object.id)) {
        duplicates.add(object.id);
      }
      seen.add(object.id);
    }

    if (duplicates.size > 0) {
      throw new BadRequestException(
        `Duplicate object IDs: ${Array.from(duplicates).join(', ')}`,
      );
    }

    this.logger.debug('Plan structure validation passed');
  }

  /**
   * Aggregates statistics describing plan occupancy information.
   */
  getPlanStatistics(plan: ExportPlanDto) {
    const roomArea = (plan.room.W * plan.room.H) / 1_000_000;
    const objectsByType = plan.objects.reduce<Record<string, number>>(
      (acc, object) => {
        acc[object.type] = (acc[object.type] || 0) + 1;
        return acc;
      },
      {},
    );

    const totalObjectArea = plan.objects.reduce((sum, object) => {
      return sum + (object.rect.W * object.rect.H) / 1_000_000;
    }, 0);

    return {
      roomArea: Math.round(roomArea * 100) / 100,
      objectsCount: plan.objects.length,
      objectsByType,
      totalObjectArea: Math.round(totalObjectArea * 100) / 100,
      occupancyRate: roomArea === 0 ? 0 : Math.round((totalObjectArea / roomArea) * 100),
    };
  }
}
