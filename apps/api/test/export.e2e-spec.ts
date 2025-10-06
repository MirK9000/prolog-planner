import { INestApplication, ValidationPipe } from '@nestjs/common';
import { Test, TestingModule } from '@nestjs/testing';
import * as request from 'supertest';
import { AppModule } from '../src/app.module';
import { HttpExceptionFilter } from '../src/common/filters/http-exception.filter';

describe('Export (e2e)', () => {
  let app: INestApplication;

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();

    app = moduleFixture.createNestApplication();
    app.useGlobalPipes(new ValidationPipe());
    app.useGlobalFilters(new HttpExceptionFilter());

    await app.init();
  });

  afterEach(async () => {
    await app.close();
  });

  describe('/export/plan.pl (POST)', () => {
    it('exports valid plan', async () => {
      const plan = {
        room: { W: 10000, H: 8000 },
        objects: [
          {
            id: 'door-1',
            type: 'door',
            rect: { X: 0, Y: 2000, W: 100, H: 900 },
            properties: [],
          },
        ],
      };

      const response = await request(app.getHttpServer())
        .post('/export/plan.pl')
        .send(plan)
        .expect(200)
        .expect('Content-Type', /text\/plain/);

      expect(response.text).toContain(':- module(plan, [])');
      expect(response.text).toContain('room(size(10000, 8000))');
      expect(response.text).toContain("static_object('door-1'");
    });

    it('rejects plan with invalid room dimensions', async () => {
      const plan = {
        room: { W: -100, H: 8000 },
        objects: [],
      };

      const response = await request(app.getHttpServer())
        .post('/export/plan.pl')
        .send(plan)
        .expect(400);

      expect(response.body.message).toBe('Validation failed');
      expect(response.body.errors).toBeDefined();
    });

    it('rejects object outside room', async () => {
      const plan = {
        room: { W: 10000, H: 8000 },
        objects: [
          {
            id: 'out-1',
            type: 'door',
            rect: { X: 10000, Y: 0, W: 900, H: 100 },
            properties: [],
          },
        ],
      };

      const response = await request(app.getHttpServer())
        .post('/export/plan.pl')
        .send(plan)
        .expect(400);

      expect(response.body.message).toContain('outside room boundaries');
    });

    it('handles task specification', async () => {
      const plan = {
        room: { W: 10000, H: 8000 },
        objects: [],
        task: { count: 10, size: { W: 1200, H: 800 } },
      };

      const response = await request(app.getHttpServer())
        .post('/export/plan.pl')
        .send(plan)
        .expect(200);

      expect(response.text).toContain('task(10, size(1200, 800))');
    });

    it('rejects empty body', async () => {
      await request(app.getHttpServer()).post('/export/plan.pl').send().expect(400);
    });
  });
});
