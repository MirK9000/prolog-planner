import React from 'react';
import { Circle } from 'react-konva';
import type { StaticObject } from '@planner/shared';

interface Props {
  objects: StaticObject[];
  baseX: number;
  baseY: number;
  mm2px: number;
}

export const CommsRadius: React.FC<Props> = ({ objects, baseX, baseY, mm2px }) => (
  <>
    {objects.map(o => {
      const r = o.properties.find(p => p.kind === 'radius');
      if (!r) return null;
      const cx = baseX + (o.rect.X + o.rect.W / 2) * mm2px;
      const cy = baseY + (o.rect.Y + o.rect.H / 2) * mm2px;
      return (
        <Circle
          key={'rad_' + o.id}
          x={cx}
          y={cy}
          radius={r.value * mm2px}
          stroke="#e879f9"
          dash={[4, 4]}
          listening={false}
        />
      );
    })}
  </>
);
