import { Test, TestingModule } from '@nestjs/testing';
import { ExportPlanDto } from '../dto/export-plan.dto';
import { ExportController } from '../export.controller';
import { ExportService } from '../export.service';

describe('ExportController', () => {
  let controller: ExportController;
  let service: ExportService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [ExportController],
      providers: [ExportService],
    }).compile();

    controller = module.get(ExportController);
    service = module.get(ExportService);
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });

  describe('exportPlan', () => {
    it('returns prolog payload for valid plan', () => {
      const plan: ExportPlanDto = {
        room: { W: 10000, H: 8000 },
        objects: [],
      };

      const prolog = ':- module(plan, []).\nroom(size(10000, 8000)).';

      jest.spyOn(service, 'exportToPrologFormat').mockReturnValue(prolog);
      jest.spyOn(service, 'getPlanStatistics').mockReturnValue({
        roomArea: 80,
        objectsCount: 0,
        objectsByType: {},
        totalObjectArea: 0,
        occupancyRate: 0,
      });

      const result = controller.exportPlan(plan);

      expect(result).toBe(prolog);
      expect(service.exportToPrologFormat).toHaveBeenCalledWith(plan);
      expect(service.getPlanStatistics).toHaveBeenCalledWith(plan);
    });

    it('delegates work to service layer', () => {
      const plan: ExportPlanDto = {
        room: { W: 5000, H: 4000 },
        objects: [
          {
            id: 'test-1',
            type: 'door',
            rect: { X: 0, Y: 0, W: 900, H: 100 },
            properties: [],
          },
        ],
      };

      jest.spyOn(service, 'exportToPrologFormat').mockReturnValue('code');
      jest.spyOn(service, 'getPlanStatistics').mockReturnValue({
        roomArea: 20,
        objectsCount: 1,
        objectsByType: { door: 1 },
        totalObjectArea: 0.09,
        occupancyRate: 0,
      });

      controller.exportPlan(plan);

      expect(service.exportToPrologFormat).toHaveBeenCalledWith(plan);
    });
  });
});
