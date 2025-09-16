
import type { Rect, Size } from '@planner/shared';
export const EQUIP_CLEAR_DEPTH = 1000; // свободная зона перед оборудованием (мм)

// пересечение прямоугольников (мм)
export const rIntersects = (a: Rect, b: Rect) =>
  a.X < b.X + b.W && a.X + a.W > b.X && a.Y < b.Y + b.H && a.Y + a.H > b.Y;

// клиентская версия вычисления зоны
export const computeDoorZone = (
  room: { W: number; H: number },
  door: { X: number; Y: number; W: number; H: number }
) => {
  const depth = 1200;
  const clamp = (value: number, min: number, max: number) =>
    Math.min(Math.max(value, min), max);

  const widthWithinRoom = (value: number) => Math.min(value, room.W);
  const heightWithinRoom = (value: number) => Math.min(value, room.H);

  if (door.Y === 0) {
    // верхняя стена → зона вниз
    const width = widthWithinRoom(door.W);
    const height = heightWithinRoom(depth);
    const x = clamp(door.X, 0, room.W - width);
    return { X: x, Y: 0, W: width, H: height };
  }

  if (door.Y + door.H === room.H) {
    // нижняя стена → зона вверх
    const width = widthWithinRoom(door.W);
    const height = heightWithinRoom(depth);
    const x = clamp(door.X, 0, room.W - width);
    const y = Math.max(0, room.H - height);
    return { X: x, Y: y, W: width, H: height };
  }

  if (door.X === 0) {
    // левая стена → зона вправо
    const width = widthWithinRoom(depth);
    const height = heightWithinRoom(door.H);
    const y = clamp(door.Y, 0, room.H - height);
    return { X: 0, Y: y, W: width, H: height };
  }

  if (door.X + door.W === room.W) {
    // правая стена → зона влево
    const width = widthWithinRoom(depth);
    const height = heightWithinRoom(door.H);
    const y = clamp(door.Y, 0, room.H - height);
    const x = Math.max(0, room.W - width);
    return { X: x, Y: y, W: width, H: height };
  }

  return null;
};

// зона свободного доступа для оборудования, примыкающего к стене
export const computeEquipmentZone = (
  room: { W: number; H: number },
  rect: { X: number; Y: number; W: number; H: number }
) => {
  const { X, Y, W, H } = rect;
  const d = EQUIP_CLEAR_DEPTH;
  if (Y === 0) return { X, Y: Y + H, W, H: d }; // верхняя стена → вниз
  if (Y + H === room.H) return { X, Y: Y - d, W, H: d }; // нижняя стена → вверх
  if (X === 0) return { X: X + W, Y, W: d, H }; // левая стена → вправо
  if (X + W === room.W) return { X: X - d, Y, W: d, H }; // правая стена → влево
  return null;
};
