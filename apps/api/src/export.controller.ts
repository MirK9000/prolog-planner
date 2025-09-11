import { Controller, Post, Body, Header } from '@nestjs/common';
import type { Plan } from '@planner/shared';
import { toProlog } from '@planner/serializer';
@Controller('export')
export class ExportController { @Post('plan.pl') @Header('Content-Type','text/plain; charset=utf-8') export(@Body() plan: Plan){ return toProlog(plan);} }
