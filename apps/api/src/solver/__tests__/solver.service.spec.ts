import { ConfigService } from '@nestjs/config';
import type { Plan } from '@planner/shared';
import { ExportService } from '../../export/export.service';
import { SolverService } from '../solver.service';

describe('SolverService', () => {
  let service: SolverService;

  beforeEach(() => {
    service = new SolverService(new ConfigService(), new ExportService());
  });

  it('should return INVALID_PLAN error when task is missing', async () => {
    const plan: Plan = {
      room: { W: 1000, H: 1000 },
      objects: [],
    };

    const result = await service.solvePlan(plan);

    expect(result.success).toBe(false);
    expect(result.error?.code).toBe('INVALID_PLAN');
  });
});
