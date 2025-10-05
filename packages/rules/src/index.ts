
import type { Plan, Issue, Rect } from '@planner/shared';
import {
  rectIntersects,
  rectInside,
  isOnWall,
  distanceRectToRect,
  computeDoorZone,
  computeEquipmentZone,
  computeWindowZone,
} from '@planner/geometry';

export { computeDoorZone, computeEquipmentZone, computeWindowZone } from '@planner/geometry';

export type Rule = (plan: Plan) => Issue[];

const byType = (plan: Plan, t: string) => plan.objects.filter(o => o.type === t);


export const REQUIRES_WALL_ANCHOR = new Set(['door', 'window']);

const DEFAULT_CLEARANCES_MM = {
  electrical_shield: 1000,
  net_cabinet: 1000,
  window: 1000,
};




export const ruleOutOfBounds: Rule = (plan) =>
  plan.objects
    .filter(o => !rectInside(o.rect, plan.room))
    .map(o => ({
      code: 'OUT_OF_BOUNDS',
      severity: 'error' as const,
      message: `Объект ${o.id} выходит за пределы помещения`,
      objectId: o.id,
      where: o.rect,
    }));

export const ruleIntersections: Rule = (plan) => {
  const issues: Issue[] = [];
  for (let i = 0; i < plan.objects.length; i++) {
    for (let j = i + 1; j < plan.objects.length; j++) {
      const a = plan.objects[i], b = plan.objects[j];
      if (rectIntersects(a.rect, b.rect)) {
        issues.push({
          code: 'INTERSECTION',
          severity: 'error' as const,
          message: `Пересечение объектов ${a.id} и ${b.id}`,
          objectId: a.id,
          where: {
            X: Math.max(a.rect.X, b.rect.X),
            Y: Math.max(a.rect.Y, b.rect.Y),
            W: Math.min(a.rect.X + a.rect.W, b.rect.X + b.rect.W) - Math.max(a.rect.X, b.rect.X),
            H: Math.min(a.rect.Y + a.rect.H, b.rect.Y + b.rect.H) - Math.max(a.rect.Y, b.rect.Y),
          },
        });
      }
    }
  }
  return issues;
};




export const ruleWindowsOnWall: Rule = (plan) =>
  byType(plan, 'window')
    .filter(o => !isOnWall(o.rect, plan.room))
    .map(o => ({
      code: 'WINDOW_NOT_ON_WALL',
      severity: 'error' as const,
      message: `Окно ${o.id} должно лежать на стене`,
      objectId: o.id,
      where: o.rect,
    }));

export const ruleDoorsOnWall: Rule = (plan) =>
  byType(plan, 'door')
    .filter(o => !isOnWall(o.rect, plan.room))
    .map(o => ({
      code: 'DOOR_NOT_ON_WALL',
      severity: 'error' as const,
      message: `Дверь ${o.id} должна лежать на стене`,
      objectId: o.id,
      where: o.rect,
    }));

export const ruleDoorWidth: Rule = (plan) =>
  byType(plan, 'door')
    .filter(o => o.rect.W < 1200)
    .map(o => ({
      code: 'DOOR_WIDTH_TOO_NARROW',
      severity: 'error' as const,
      message: `Дверь ${o.id}: ширина ${o.rect.W} мм, требуется ≥ 1200 мм`,
      objectId: o.id,
      where: o.rect,
    }));

export const ruleExtinguisherMinCount: Rule = (plan) => {
  const count = byType(plan, 'fire_extinguisher').length;
  return count >= 2
    ? []
    : [{
        code: 'EXTINGUISHER_COUNT_MIN',
        severity: 'error' as const,
        message: `Нужно ≥ 2 огнетушителей, сейчас ${count}`,
      }];
};

export const ruleExtinguisherCoverage: Rule = (plan) => {
  const exts = byType(plan, 'fire_extinguisher').map(e => e.rect);
  const workplaces = byType(plan, 'workplace').map(o => o.rect);
  const MAX = 20000; // mm
  const issues: Issue[] = [];
  for (let i = 0; i < workplaces.length; i++) {
    const w = workplaces[i];
    const min = exts.length ? Math.min(...exts.map(e => distanceRectToRect(w, e))) : Infinity;
    if (min > MAX) {
      issues.push({
        code: 'EXTINGUISHER_COVERAGE',
        severity: 'error' as const,
        message: `Рабочая зона ${i + 1}: до ближайшего огнетушителя ${Math.round(min)} мм (> 20000)`,
        where: w,
      });
    }
  }
  return issues;
};

export const ruleEquipmentAccess: Rule = (plan) => {
  const types = new Set(['electrical_shield', 'net_cabinet']);
  const zones = plan.objects
    .filter(o => types.has(o.type))
    .map(o => ({
      id: o.id,
      z: computeEquipmentZone(
        plan.room,
        o.rect,
        o.type === 'electrical_shield'
          ? DEFAULT_CLEARANCES_MM.electrical_shield
          : DEFAULT_CLEARANCES_MM.net_cabinet
      ),
    }))
    .filter((x): x is { id: string; z: Rect } => !!x.z);
  const issues: Issue[] = [];
  for (const { id, z } of zones) {
    for (const o of plan.objects) {
      if (o.id === id) continue;
      if (rectIntersects(o.rect, z)) {
        issues.push({
          code: 'EQUIPMENT_ACCESS_CONFLICT',
          severity: 'error' as const,
          message: `Зона доступа к оборудованию ${id} перекрыта объектом ${o.id}`,
          objectId: o.id,
          where: {
            X: Math.max(o.rect.X, z.X),
            Y: Math.max(o.rect.Y, z.Y),
            W: Math.min(o.rect.X + o.rect.W, z.X + z.W) - Math.max(o.rect.X, z.X),
            H: Math.min(o.rect.Y + o.rect.H, z.Y + z.H) - Math.max(o.rect.Y, z.Y),
          },
        });
      }
    }
  }
  return issues;
};

export const ruleWindowAccess: Rule = (plan) => {
  const zones = byType(plan, 'window')
    .map(o => ({
      id: o.id,
      z: computeWindowZone(plan.room, o.rect, DEFAULT_CLEARANCES_MM.window),
    }))
    .filter((x): x is { id: string; z: Rect } => !!x.z);
  const issues: Issue[] = [];
  for (const { id, z } of zones) {
    for (const o of plan.objects) {
      if (o.id === id) continue;
      if (rectIntersects(o.rect, z)) {
        issues.push({
          code: 'WINDOW_ACCESS_CONFLICT',
          severity: 'error' as const,
          message: `Зона доступа к окну ${id} перекрыта объектом ${o.id}`,
          objectId: o.id,
          where: {
            X: Math.max(o.rect.X, z.X),
            Y: Math.max(o.rect.Y, z.Y),
            W: Math.min(o.rect.X + o.rect.W, z.X + z.W) - Math.max(o.rect.X, z.X),
            H: Math.min(o.rect.Y + o.rect.H, z.Y + z.H) - Math.max(o.rect.Y, z.Y),
          },
        });
      }
    }
  }
  return issues;
};


export const runAllBasicRules: Rule = (plan) => [
  ...ruleOutOfBounds(plan),
  ...ruleIntersections(plan),
  ...ruleWindowsOnWall(plan),
  ...ruleDoorsOnWall(plan),
  ...ruleDoorWidth(plan),
  ...ruleExtinguisherMinCount(plan),
  ...ruleExtinguisherCoverage(plan),
  ...ruleEquipmentAccess(plan),
  ...ruleWindowAccess(plan),
];
