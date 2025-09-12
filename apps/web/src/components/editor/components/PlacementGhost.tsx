import React from 'react';
import { Rect } from 'react-konva';

interface Ghost {
  X: number;
  Y: number;
  W: number;
  H: number;
}
interface Props {
  ghost: Ghost | null;
  baseX: number;
  baseY: number;
  mm2px: number;
  forbidden: boolean;
}

export const PlacementGhost: React.FC<Props> = ({
  ghost,
  baseX,
  baseY,
  mm2px,
  forbidden,
}) => {
  if (!ghost) return null;
  return (
    <Rect
      x={baseX + ghost.X * mm2px}
      y={baseY + ghost.Y * mm2px}
      width={ghost.W * mm2px}
      height={ghost.H * mm2px}
      fill={forbidden ? 'rgba(239,68,68,0.25)' : 'rgba(34,197,94,0.2)'}
      stroke={forbidden ? '#ef4444' : '#16a34a'}
      strokeWidth={2}
      dash={[6, 4]}
      listening={false}
    />
  );
};
