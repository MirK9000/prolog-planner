import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { appConfig } from './config/app.config';
import { ExportModule } from './export/export.module';
import { SolverModule } from './solver/solver.module';

/**
 * Root NestJS application module.
 */
@Module({
  imports: [
    ConfigModule.forRoot({
      isGlobal: true,
      load: [() => appConfig],
    }),
    ExportModule,
    SolverModule,
  ],
})
export class AppModule {}
