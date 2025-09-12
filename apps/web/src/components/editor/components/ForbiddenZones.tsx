import React from 'react';
import { Rect } from 'react-konva';

interface Zone {
  id: string;
  z: { X: number; Y: number; W: number; H: number };
}
interface Props {
  zones: Zone[];
  baseX: number;
  baseY: number;
  mm2px: number;
}

export const ForbiddenZones: React.FC<Props> = ({ zones, baseX, baseY, mm2px }) => (
  <>
    {zones.map(({ id, z }) => (
      <Rect
        key={'dz_' + id}
        x={baseX + z.X * mm2px}
        y={baseY + z.Y * mm2px}
        width={z.W * mm2px}
        height={z.H * mm2px}
        fill="rgba(239,68,68,0.12)"
        stroke="#ef4444"
        dash={[6, 4]}
        listening={false}
      />
    ))}
  </>
);
