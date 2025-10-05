import type { Rect, Size } from '@planner/shared';

/**
 * Проверяет пересечение двух прямоугольников.
 * @param a Первый прямоугольник.
 * @param b Второй прямоугольник.
 * @returns true, если прямоугольники пересекаются.
 */
export const rectIntersects = (a: Rect, b: Rect): boolean =>
  a.X < b.X + b.W && a.X + a.W > b.X && a.Y < b.Y + b.H && a.Y + a.H > b.Y;

/**
 * Проверяет, находится ли прямоугольник внутри помещения.
 * @param rect Прямоугольник.
 * @param room Размеры помещения.
 * @returns true, если прямоугольник полностью внутри помещения.
 */
export const rectInside = (rect: Rect, room: Size): boolean =>
  rect.X >= 0 &&
  rect.Y >= 0 &&
  rect.X + rect.W <= room.W &&
  rect.Y + rect.H <= room.H;

/**
 * Проверяет, примыкает ли прямоугольник к стене помещения.
 * @param rect Прямоугольник объекта.
 * @param room Размеры помещения.
 * @returns true, если объект лежит на одной из стен помещения.
 */
export const isOnWall = (rect: Rect, room: Size): boolean =>
  rect.X === 0 ||
  rect.Y === 0 ||
  rect.X + rect.W === room.W ||
  rect.Y + rect.H === room.H;

/**
 * Вычисляет расстояние между двумя прямоугольниками.
 * @param a Первый прямоугольник.
 * @param b Второй прямоугольник.
 * @returns Расстояние между прямоугольниками.
 */
export const distanceRectToRect = (a: Rect, b: Rect): number => {
  const dx = Math.max(b.X - (a.X + a.W), a.X - (b.X + b.W), 0);
  const dy = Math.max(b.Y - (a.Y + a.H), a.Y - (b.Y + b.H), 0);
  return Math.hypot(dx, dy);
};

/**
 * Преобразует миллиметры в пиксели с учётом масштаба.
 * @param mm Значение в миллиметрах.
 * @param scale Масштаб преобразования.
 * @returns Значение в пикселях.
 */
export const mmToPx = (mm: number, scale = 0.1): number => mm * scale;

/**
 * Преобразует пиксели в миллиметры с учётом масштаба.
 * @param px Значение в пикселях.
 * @param scale Масштаб преобразования.
 * @returns Значение в миллиметрах.
 */
export const pxToMm = (px: number, scale = 0.1): number => px / scale;

/**
 * Ограничивает значение диапазоном [min, max].
 * @param value Исходное значение.
 * @param min Минимально допустимое значение.
 * @param max Максимально допустимое значение.
 * @returns Ограниченное значение.
 */
export const clamp = (value: number, min: number, max: number): number =>
  Math.min(Math.max(value, min), max);

/**
 * Приводит значение к ближайшему шагу.
 * @param value Значение для приведения.
 * @param step Шаг сетки.
 * @returns Приведённое значение.
 */
export const snap = (value: number, step: number): number =>
  Math.round(value / step) * step;
