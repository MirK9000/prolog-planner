import {
  clamp,
  distanceRectToRect,
  isOnWall,
  mmToPx,
  pxToMm,
  rectInside,
  rectIntersects,
  snap,
} from '..';

const rect = (X: number, Y: number, W: number, H: number) => ({ X, Y, W, H });

describe('calculations', () => {
  describe('rectIntersects', () => {
    it('определяет пересечение прямоугольников', () => {
      expect(rectIntersects(rect(0, 0, 100, 100), rect(50, 50, 100, 100))).toBe(true);
    });

    it('определяет отсутствие пересечения', () => {
      expect(rectIntersects(rect(0, 0, 100, 100), rect(200, 200, 50, 50))).toBe(false);
    });

    it('возвращает false при касании границ', () => {
      expect(rectIntersects(rect(0, 0, 100, 100), rect(100, 0, 100, 100))).toBe(false);
    });
  });

  describe('rectInside', () => {
    const room = { W: 200, H: 200 };

    it('возвращает true для прямоугольника внутри помещения', () => {
      expect(rectInside(rect(10, 10, 50, 50), room)).toBe(true);
    });

    it('возвращает false если выходит за границы помещения', () => {
      expect(rectInside(rect(180, 10, 50, 50), room)).toBe(false);
    });
  });

  describe('isOnWall', () => {
    const room = { W: 200, H: 200 };

    it('возвращает true для объекта на стене', () => {
      expect(isOnWall(rect(0, 50, 20, 40), room)).toBe(true);
    });

    it('возвращает false для объекта внутри помещения', () => {
      expect(isOnWall(rect(50, 50, 20, 20), room)).toBe(false);
    });
  });

  describe('distanceRectToRect', () => {
    it('возвращает 0 для пересекающихся прямоугольников', () => {
      expect(distanceRectToRect(rect(0, 0, 100, 100), rect(50, 50, 100, 100))).toBe(0);
    });

    it('вычисляет расстояние по диагонали', () => {
      const distance = distanceRectToRect(rect(0, 0, 50, 50), rect(100, 100, 50, 50));
      expect(distance).toBeCloseTo(Math.hypot(50, 50));
    });
  });

  describe('mm/px conversion', () => {
    it('конвертирует миллиметры в пиксели и обратно', () => {
      const px = mmToPx(100, 0.5);
      expect(px).toBe(50);
      expect(pxToMm(px, 0.5)).toBe(100);
    });
  });

  describe('clamp', () => {
    it('ограничивает значение снизу', () => {
      expect(clamp(5, 10, 20)).toBe(10);
    });

    it('ограничивает значение сверху', () => {
      expect(clamp(25, 10, 20)).toBe(20);
    });

    it('возвращает значение внутри диапазона', () => {
      expect(clamp(15, 10, 20)).toBe(15);
    });
  });

  describe('snap', () => {
    it('приводит значение к ближайшему шагу', () => {
      expect(snap(12.3, 5)).toBe(10);
      expect(snap(12.6, 5)).toBe(15);
    });
  });
});
