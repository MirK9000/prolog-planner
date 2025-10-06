import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { SolverController } from './solver.controller';
import { SolverService } from './solver.service';
import { ExportModule } from '../export/export.module';

/**
 * Module encapsulating solver REST API and business logic.
 */
@Module({
  imports: [ConfigModule, ExportModule],
  controllers: [SolverController],
  providers: [SolverService],
  exports: [SolverService],
})
export class SolverModule {}
