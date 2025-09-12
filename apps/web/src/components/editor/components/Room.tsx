import React from 'react';
import { Rect } from 'react-konva';

interface Props {
  baseX: number;
  baseY: number;
  roomWpx: number;
  roomHpx: number;
}

export const Room: React.FC<Props> = ({ baseX, baseY, roomWpx, roomHpx }) => (
  <Rect x={baseX} y={baseY} width={roomWpx} height={roomHpx} stroke="#999" />
);
