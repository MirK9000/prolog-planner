import { BadRequestException } from '@nestjs/common';
import { Test, TestingModule } from '@nestjs/testing';
import { ExportPlanDto } from '../dto/export-plan.dto';
import { ExportService } from '../export.service';

describe('ExportService', () => {
  let service: ExportService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [ExportService],
    }).compile();

    service = module.get(ExportService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });

  describe('exportToPrologFormat', () => {
    it('exports plan to prolog code', () => {
      const plan: ExportPlanDto = {
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

      const result = service.exportToPrologFormat(plan);

      expect(result).toContain(':- module(plan, [])');
      expect(result).toContain('room(size(10000, 8000))');
      expect(result).toContain("static_object('door-1'");
    });

    it('includes task specification when provided', () => {
      const plan: ExportPlanDto = {
        room: { W: 10000, H: 8000 },
        objects: [],
        task: { count: 10, size: { W: 1200, H: 800 } },
      };

      const result = service.exportToPrologFormat(plan);

      expect(result).toContain('task(10, size(1200, 800))');
    });

    it('throws error when object is outside room', () => {
      const plan: ExportPlanDto = {
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

      expect(() => service.exportToPrologFormat(plan)).toThrow(BadRequestException);
    });
  });

  describe('validatePlanStructure', () => {
    it('allows valid plan', () => {
      const plan: ExportPlanDto = {
        room: { W: 10000, H: 8000 },
        objects: [
          {
            id: 'obj-1',
            type: 'door',
            rect: { X: 0, Y: 0, W: 900, H: 100 },
            properties: [],
          },
        ],
      };

      expect(() => service.validatePlanStructure(plan)).not.toThrow();
    });

    it('rejects objects outside room', () => {
      const plan: ExportPlanDto = {
        room: { W: 10000, H: 8000 },
        objects: [
          {
            id: 'obj-1',
            type: 'door',
            rect: { X: 10001, Y: 0, W: 900, H: 100 },
            properties: [],
          },
        ],
      };

      expect(() => service.validatePlanStructure(plan)).toThrow(
        'Objects outside room boundaries',
      );
    });

    it('rejects duplicate ids', () => {
      const plan: ExportPlanDto = {
        room: { W: 10000, H: 8000 },
        objects: [
          {
            id: 'obj-1',
            type: 'door',
            rect: { X: 0, Y: 0, W: 900, H: 100 },
            properties: [],
          },
          {
            id: 'obj-1',
            type: 'window',
            rect: { X: 1000, Y: 0, W: 1500, H: 200 },
            properties: [],
          },
        ],
      };

      expect(() => service.validatePlanStructure(plan)).toThrow(
        'Duplicate object IDs',
      );
    });
  });

  describe('getPlanStatistics', () => {
    it('computes statistics summary', () => {
      const plan: ExportPlanDto = {
        room: { W: 10000, H: 8000 },
        objects: [
          {
            id: 'door-1',
            type: 'door',
            rect: { X: 0, Y: 0, W: 900, H: 100 },
            properties: [],
          },
          {
            id: 'door-2',
            type: 'door',
            rect: { X: 1000, Y: 0, W: 900, H: 100 },
            properties: [],
          },
          {
            id: 'window-1',
            type: 'window',
            rect: { X: 2000, Y: 0, W: 1500, H: 200 },
            properties: [],
          },
        ],
      };

      const stats = service.getPlanStatistics(plan);

      expect(stats.roomArea).toBe(80);
      expect(stats.objectsCount).toBe(3);
      expect(stats.objectsByType).toEqual({ door: 2, window: 1 });
      expect(stats.totalObjectArea).toBeGreaterThan(0);
      expect(stats.occupancyRate).toBeGreaterThan(0);
    });
  });
});
