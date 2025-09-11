
import type { Plan, StaticObject } from '@planner/shared';

const propToProlog = (p: StaticObject['properties'][number]): string => {
  if (p.kind === 'capacity') return `capacity(${p.value})`;
  if (p.kind === 'status') return `status(${p.value})`;
  if (p.kind === 'type') return `type(${p.value})`;
  return '';
};

export const toProlog = (plan: Plan): string => {
  const lines: string[] = [];
  lines.push(`room(size(${plan.room.W}, ${plan.room.H})).`);
  for (const o of plan.objects) {
    const props = o.properties?.map(propToProlog).filter(Boolean).join(', ');
    const propsList = props ? `[${props}]` : '[]';
    const r = o.rect;
    lines.push(`static_object(${o.id}, ${o.type}, rect(${r.X}, ${r.Y}, ${r.W}, ${r.H}), ${propsList}).`);
  }
  if (plan.task) {
    lines.push(`\n% task spec`);
    lines.push(`task(${plan.task.count}, size(${plan.task.size.W}, ${plan.task.size.H})).`);
  }
  return lines.join('\n');
};
