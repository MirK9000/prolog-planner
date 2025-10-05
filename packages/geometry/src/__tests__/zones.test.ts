import { computeDoorZone, computeEquipmentZone, computeWindowZone } from '../zones';
import type { Rect, Size } from '@planner/shared';

describe('zones', () => {
  const room: Size = { W: 10000, H: 8000 };

  describe('computeDoorZone', () => {
    it('возвращает зону для двери на верхней стене', () => {
      const door: Rect = { X: 1000, Y: 0, W: 900, H: 100 };
      expect(computeDoorZone(room, door)).toEqual({ X: 1000, Y: 0, W: 900, H: 1200 });
    });

    it('возвращает зону для двери на левой стене', () => {
      const door: Rect = { X: 0, Y: 2000, W: 100, H: 900 };
      expect(computeDoorZone(room, door)).toEqual({ X: 0, Y: 2000, W: 1200, H: 900 });
    });

    it('возвращает null если дверь не на стене', () => {
      const door: Rect = { X: 1000, Y: 1000, W: 900, H: 100 };
      expect(computeDoorZone(room, door)).toBeNull();
    });

    it('ограничивает зону границами помещения', () => {
      const door: Rect = { X: 9500, Y: 0, W: 900, H: 100 };
      const zone = computeDoorZone(room, door);
      expect(zone).not.toBeNull();
      expect(zone!.X + zone!.W).toBeLessThanOrEqual(room.W);
    });
  });

  describe('computeEquipmentZone', () => {
    it('строит зону доступа внутрь помещения', () => {
      const equipment: Rect = { X: 0, Y: 1000, W: 500, H: 800 };
      expect(computeEquipmentZone(room, equipment, 1000)).toEqual({
        X: 500,
        Y: 1000,
        W: 1000,
        H: 800,
      });
    });

    it('возвращает null если оборудование не на стене', () => {
      const equipment: Rect = { X: 1000, Y: 1000, W: 500, H: 800 };
      expect(computeEquipmentZone(room, equipment, 1000)).toBeNull();
    });
  });

  describe('computeWindowZone', () => {
    it('строит зону доступа для окна на верхней стене', () => {
      const windowRect: Rect = { X: 2000, Y: 0, W: 1500, H: 200 };
      expect(computeWindowZone(room, windowRect, 1000)).toEqual({
        X: 2000,
        Y: 0,
        W: 1500,
        H: 1000,
      });
    });

    it('ограничивает зону окна размерами помещения', () => {
      const windowRect: Rect = { X: 9500, Y: 0, W: 1500, H: 200 };
      const zone = computeWindowZone(room, windowRect, 2000);
      expect(zone).not.toBeNull();
      expect(zone!.X + zone!.W).toBeLessThanOrEqual(room.W);
    });

    it('возвращает null если окно не на стене', () => {
      const windowRect: Rect = { X: 1000, Y: 1000, W: 500, H: 200 };
      expect(computeWindowZone(room, windowRect, 1000)).toBeNull();
    });
  });
});
