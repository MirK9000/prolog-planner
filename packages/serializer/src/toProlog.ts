
import type { Plan, Property } from '@planner/shared';

const propToProlog = (p: Property): string => {
  switch (p.kind) {
    case 'capacity':
      return `capacity(${p.value})`;
    case 'status':
      return `status(${p.value})`;
    case 'type':
      return `type(${p.value})`;
    default:
      return '';
  }
};

export const toProlog = (plan: Plan): string => {
  const lines: string[] = [];
  lines.push(':- module(plan, []).', '');
  lines.push(`room(size(${plan.room.W}, ${plan.room.H})).`);
  for (const o of plan.objects) {
    const props = o.properties?.map(propToProlog).filter(Boolean).join(', ');
    const propsList = props ? `[${props}]` : '[]';
    const r = o.rect;
    lines.push(`static_object('${o.id}', ${o.type}, rect(${r.X}, ${r.Y}, ${r.W}, ${r.H}), ${propsList}).`);
  }
  if (plan.task) {
    lines.push(`\n% task spec`);
    lines.push(`task(${plan.task.count}, size(${plan.task.size.W}, ${plan.task.size.H})).`);
  }
  return lines.join('\n');
};
