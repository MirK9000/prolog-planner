import 'reflect-metadata';
import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
(async function bootstrap(){ const app = await NestFactory.create(AppModule,{cors:true}); await app.listen(3333); console.log('API http://localhost:3333'); })();
