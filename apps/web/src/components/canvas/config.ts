// apps/web/src/components/canvas/config.ts
import type { StaticObject } from '@planner/shared';

// Визуальные константы
export const PX_PER_MM = 0.1;
export const GRID_MM = 100;
export const MIN_SIZE_MM = 100;
export const PADDING_PX = 20;

// Текстовые константы
export const TYPE_LABEL: Record<string, string> = {
  door: 'Дверь',
  window: 'Окно',
  column: 'Колонна',
  workplace: 'РМ',
  fire_extinguisher: 'Огнетуш.',
  fire_alarm: 'Пож. изв.',
  electrical_shield: 'Эл. щит',
  comms_block: 'Связь',
  net_cabinet: 'Сет. шкаф',
  cabinet: 'Шкаф',
  wall: 'Стена',
};

export const TYPE_SHORT_LABEL: Record<string, string> = {
  workplace: 'РМ', door: 'Дверь', window: 'Окно', column: 'Кол.',
  fire_extinguisher: 'Огн.', fire_alarm: 'Изв.', electrical_shield: 'Щит',
  comms_block: 'Связь', net_cabinet: 'Сет.', cabinet: 'Шкаф', wall: 'Ст.',
};

// Визуальные стили
export const TYPE_COLOR: Record<string, { fill: string; stroke: string }> = {
  door: { fill: '#f59e0b33', stroke: '#f59e0b' },
  window: { fill: '#38bdf833', stroke: '#38bdf8' },
  column: { fill: '#94a3b833', stroke: '#94a3b8' },
  workplace: { fill: '#22c55e33', stroke: '#22c55e' },
  fire_extinguisher: { fill: '#ef444433', stroke: '#ef4444' },
  fire_alarm: { fill: '#f9731633', stroke: '#f97316' },
  electrical_shield: { fill: '#06b6d433', stroke: '#06b6d4' },
  comms_block: { fill: '#e879f933', stroke: '#e879f9' },
  net_cabinet: { fill: '#10b98133', stroke: '#10b981' },
  cabinet: { fill: '#d946ef33', stroke: '#d946ef' },
  wall: { fill: '#9ca3af33', stroke: '#9ca3af' },
};

// Логика объектов
export const DEFAULT_SIZE_MM: Record<string, { W: number; H: number }> = {
  workplace: { W: 1200, H: 800 }, door: { W: 900, H: 200 }, window: { W: 2000, H: 200 },
  column: { W: 1000, H: 1000 }, fire_extinguisher: { W: 200, H: 200 }, fire_alarm: { W: 200, H: 200 },
  electrical_shield: { W: 800, H: 200 }, cabinet: { W: 1000, H: 500 }, net_cabinet: { W: 800, H: 600 },
  comms_block: { W: 400, H: 200 },
};

export const REQUIRES_WALL_ANCHOR = new Set(['door', 'window']);