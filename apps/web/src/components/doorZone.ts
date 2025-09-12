export const DOOR_CLEAR_FACTOR = 1.5; // квадрат со стороной 1.5*W двери, направлен внутрь

// пересечение прямоугольников (мм)
export const rIntersects = (
  a: { X: number; Y: number; W: number; H: number },
  b: { X: number; Y: number; W: number; H: number }
) => a.X < b.X + b.W && a.X + a.W > b.X && a.Y < b.Y + b.H && a.Y + a.H > b.Y;

// клиентская версия вычисления зоны
export const computeDoorZone = (
  room: { W: number; H: number },
  door: { X: number; Y: number; W: number; H: number }
) => {
  const { X, Y, W, H } = door;
  const side = Math.round(DOOR_CLEAR_FACTOR * W);
  const cx = X + W / 2;
  const cy = Y + H / 2;
  const clamp01 = (v: number, min: number, max: number) => Math.max(min, Math.min(max, v));

  if (X === 0) {
    // левая стена → зона вправо
    return {
      X: X + W,
      Y: clamp01(Math.round(cy - side / 2), 0, room.H - side),
      W: Math.min(side, room.W - (X + W)),
      H: Math.min(side, room.H),
    };
  }
  if (X + W === room.W) {
    // правая стена → зона влево
    return {
      X: Math.max(0, room.W - W - side),
      Y: clamp01(Math.round(cy - side / 2), 0, room.H - side),
      W: Math.min(side, room.W - W),
      H: Math.min(side, room.H),
    };
  }
  if (Y === 0) {
    // верхняя стена → зона вниз
    return {
      X: clamp01(Math.round(cx - side / 2), 0, room.W - side),
      Y: Y + H,
      W: Math.min(side, room.W),
      H: Math.min(side, room.H - (Y + H)),
    };
  }
  if (Y + H === room.H) {
    // нижняя стена → зона вверх
    return {
      X: clamp01(Math.round(cx - side / 2), 0, room.W - side),
      Y: Math.max(0, room.H - H - side),
      W: Math.min(side, room.W),
      H: Math.min(side, room.H - H),
    };
  }
  return null;
};
