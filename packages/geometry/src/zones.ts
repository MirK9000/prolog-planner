import type { Rect, Size } from '@planner/shared';
import { clamp } from './calculations';

const DOOR_DEPTH_MM = 1200;

/**
 * Вычисляет зону открытия двери (1200 мм внутрь помещения).
 * @param room Размеры помещения.
 * @param doorRect Координаты и размеры двери.
 * @returns Прямоугольник зоны или null, если дверь не на стене.
 */
export const computeDoorZone = (room: Size, doorRect: Rect): Rect | null => {
  const { X, Y, W, H } = doorRect;
  const depth = DOOR_DEPTH_MM;

  const widthWithinRoom = (value: number) => Math.min(value, room.W);
  const heightWithinRoom = (value: number) => Math.min(value, room.H);

  if (Y === 0) {
    const width = widthWithinRoom(W);
    const height = heightWithinRoom(depth);
    const x = clamp(X, 0, room.W - width);
    return { X: x, Y: 0, W: width, H: height };
  }

  if (Y + H === room.H) {
    const width = widthWithinRoom(W);
    const height = heightWithinRoom(depth);
    const x = clamp(X, 0, room.W - width);
    const y = Math.max(0, room.H - height);
    return { X: x, Y: y, W: width, H: height };
  }

  if (X === 0) {
    const width = widthWithinRoom(depth);
    const height = heightWithinRoom(H);
    const y = clamp(Y, 0, room.H - height);
    return { X: 0, Y: y, W: width, H: height };
  }

  if (X + W === room.W) {
    const width = widthWithinRoom(depth);
    const height = heightWithinRoom(H);
    const y = clamp(Y, 0, room.H - height);
    const x = Math.max(0, room.W - width);
    return { X: x, Y: y, W: width, H: height };
  }

  return null;
};

/**
 * Вычисляет зону доступа к оборудованию, примыкающему к стене.
 * @param room Размеры помещения.
 * @param rect Координаты оборудования.
 * @param depth Глубина зоны доступа.
 * @returns Прямоугольник зоны или null, если оборудование не на стене.
 */
export const computeEquipmentZone = (
  room: Size,
  rect: Rect,
  depth: number
): Rect | null => {
  const { X, Y, W, H } = rect;

  if (Y === 0) return { X, Y: Y + H, W, H: depth };
  if (Y + H === room.H) return { X, Y: Y - depth, W, H: depth };
  if (X === 0) return { X: X + W, Y, W: depth, H };
  if (X + W === room.W) return { X: X - depth, Y, W: depth, H };

  return null;
};

/**
 * Вычисляет зону доступа к окну.
 * @param room Размеры помещения.
 * @param windowRect Координаты окна.
 * @param depth Глубина зоны доступа.
 * @returns Прямоугольник зоны или null, если окно не на стене.
 */
export const computeWindowZone = (
  room: Size,
  windowRect: Rect,
  depth: number
): Rect | null => {
  const { X, Y, W, H } = windowRect;

  const widthWithinRoom = (value: number) => Math.min(value, room.W);
  const heightWithinRoom = (value: number) => Math.min(value, room.H);

  if (Y === 0) {
    const width = widthWithinRoom(W);
    const height = heightWithinRoom(depth);
    const x = clamp(X, 0, room.W - width);
    return { X: x, Y: 0, W: width, H: height };
  }

  if (Y + H === room.H) {
    const width = widthWithinRoom(W);
    const height = heightWithinRoom(depth);
    const x = clamp(X, 0, room.W - width);
    const y = Math.max(0, room.H - height);
    return { X: x, Y: y, W: width, H: height };
  }

  if (X === 0) {
    const width = widthWithinRoom(depth);
    const height = heightWithinRoom(H);
    const y = clamp(Y, 0, room.H - height);
    return { X: 0, Y: y, W: width, H: height };
  }

  if (X + W === room.W) {
    const width = widthWithinRoom(depth);
    const height = heightWithinRoom(H);
    const y = clamp(Y, 0, room.H - height);
    const x = Math.max(0, room.W - width);
    return { X: x, Y: y, W: width, H: height };
  }

  return null;
};
