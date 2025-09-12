import React from 'react';
import { Line } from 'react-konva';

interface Props {
  baseX: number;
  baseY: number;
  room: { W: number; H: number };
  mm2px: number;
  grid: number;
}

export const Grid: React.FC<Props> = ({ baseX, baseY, room, mm2px, grid }) => {
  const lines = [] as React.ReactNode[];
  for (let mm = 0; mm <= room.W; mm += grid) {
    const x = baseX + mm * mm2px;
    lines.push(
      <Line key={'v' + mm} points={[x, baseY, x, baseY + room.H * mm2px]} stroke="#eee" />
    );
  }
  for (let mm = 0; mm <= room.H; mm += grid) {
    const y = baseY + mm * mm2px;
    lines.push(
      <Line key={'h' + mm} points={[baseX, y, baseX + room.W * mm2px, y]} stroke="#eee" />
    );
  }
  return <>{lines}</>;
};
