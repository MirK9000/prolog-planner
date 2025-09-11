import { Module } from '@nestjs/common';
import { ExportController } from './export.controller';

@Module({ imports: [], controllers: [ExportController], providers: [] })
export class AppModule {}
