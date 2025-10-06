import { IsNumber, IsOptional, Max, Min } from 'class-validator';
import { ExportPlanDto } from '../../export/dto/export-plan.dto';

/**
 * DTO used to validate payload for solver endpoint.
 */
export class SolvePlanDto extends ExportPlanDto {
  @IsOptional()
  @IsNumber()
  @Min(5)
  @Max(300)
  timeout?: number;
}
