import {
  Body,
  Controller,
  Header,
  HttpCode,
  HttpStatus,
  Logger,
  Post,
} from '@nestjs/common';
import { ExportPlanDto } from './dto/export-plan.dto';
import { ExportService } from './export.service';

/**
 * HTTP endpoints for exporting planner data to Prolog format.
 */
@Controller('export')
export class ExportController {
  private readonly logger = new Logger(ExportController.name);

  constructor(private readonly exportService: ExportService) {}

  /**
   * POST /export/plan.pl
   * Accepts plan payload and returns Prolog serialization.
   */
  @Post('plan.pl')
  @HttpCode(HttpStatus.OK)
  @Header('Content-Type', 'text/plain; charset=utf-8')
  exportPlan(@Body() plan: ExportPlanDto): string {
    this.logger.log(
      `Received export request for plan with ${plan.objects.length} objects`,
    );

    const prologCode = this.exportService.exportToPrologFormat(plan);
    const stats = this.exportService.getPlanStatistics(plan);
    this.logger.log(`Export completed. Stats: ${JSON.stringify(stats)}`);

    return prologCode;
  }
}
