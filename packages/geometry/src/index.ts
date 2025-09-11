
import type { Rect, Size } from '@planner/shared';

export const rectIntersects = (a: Rect, b: Rect): boolean =>
  a.X < b.X + b.W && a.X + a.W > b.X && a.Y < b.Y + b.H && a.Y + a.H > b.Y;

export const rectInside = (a: Rect, room: Size): boolean =>
  a.X >= 0 && a.Y >= 0 && a.X + a.W <= room.W && a.Y + a.H <= room.H;

export const isOnWall = (r: Rect, room: Size): boolean =>
  r.X === 0 || r.Y === 0 || r.X + r.W === room.W || r.Y + r.H === room.H;

export const distanceRectToRect = (a: Rect, b: Rect): number => {
  const dx = Math.max(b.X - (a.X + a.W), a.X - (b.X + b.W), 0);
  const dy = Math.max(b.Y - (a.Y + a.H), a.Y - (b.Y + b.H), 0);
  return Math.hypot(dx, dy);
};

export const mmToPx = (mm: number, scale = 0.1) => mm * scale;
export const pxToMm = (px: number, scale = 0.1) => px / scale;

export const clamp = (v: number, min: number, max: number) => Math.max(min, Math.min(max, v));
export const snap = (v: number, step: number) => Math.round(v / step) * step;
