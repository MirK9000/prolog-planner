import React from 'react';
import { Rect, Text } from 'react-konva';
import { clamp } from '@planner/geometry';
import type { StaticObject } from '@planner/shared';
import { TYPE_COLOR, TYPE_LABEL, TYPE_SHORT } from '../config';

import { rIntersects } from '../doorZone';


interface Props {
  object: StaticObject;
  selected: boolean;
  baseX: number;
  baseY: number;
  mm2px: number;
  roomWpx: number;
  roomHpx: number;
  onClick: () => void;
  onDragMove: (node: any) => void;
  onDragEnd: (x: number, y: number) => void;
  onTransform: (node: any) => void;
  onTransformEnd: () => void;
  setRef: (ref: any) => void;
  zoom: number;
  forbiddenZones: { z: { X: number; Y: number; W: number; H: number } }[];
}

export const StaticObjectShape: React.FC<Props> = ({
  object,
  selected,
  baseX,
  baseY,
  mm2px,
  roomWpx,
  roomHpx,
  onClick,
  onDragMove,
  onDragEnd,
  onTransform,
  onTransformEnd,
  setRef,
  zoom,
  forbiddenZones,
}) => {
  const x = baseX + object.rect.X * mm2px;
  const y = baseY + object.rect.Y * mm2px;
  const w = object.rect.W * mm2px;
  const h = object.rect.H * mm2px;

  const style = TYPE_COLOR[object.type] || {
    fill: 'rgba(99,102,241,0.08)',
    stroke: '#6b7280',
  };
  const stroke = selected ? '#6366f1' : style.stroke;
  const strokeWidth = selected ? 2 : 1;
  const fill = style.fill;

  const mainLabel = selected
    ? `${TYPE_LABEL[object.type] ?? object.type} · ${object.id}`
    : TYPE_SHORT[object.type] ?? TYPE_LABEL[object.type] ?? object.type;

  const shouldShowLabel = (wpx: number, hpx: number) => {
    if (selected) return true;
    if (zoom < 0.45) return false;
    return wpx >= 50 && hpx >= 26;
  };

  const fontSizeFor = () =>
    selected ? 12 : Math.max(9, Math.min(12, 9 + (zoom - 1) * 3));
  const last = React.useRef({ x, y });

  return (
    <>
      <Rect
        ref={setRef}
        x={x}
        y={y}
        width={w}
        height={h}
        fill={fill}
        stroke={stroke}
        strokeWidth={strokeWidth}
        draggable
        dragBoundFunc={(pos) => {
          let nx = clamp(pos.x, baseX, baseX + roomWpx - w);
          let ny = clamp(pos.y, baseY, baseY + roomHpx - h);
          const rect = {
            X: Math.round((nx - baseX) / mm2px),
            Y: Math.round((ny - baseY) / mm2px),
            W: object.rect.W,
            H: object.rect.H,
          };
          const hit = forbiddenZones.some(({ z }) => rIntersects(rect, z));
          if (hit) return last.current;
          last.current = { x: nx, y: ny };
          return { x: nx, y: ny };
        }}
        onClick={onClick}
        onDragMove={(e) => onDragMove(e.target)}
        onDragEnd={(e) => onDragEnd(e.target.x(), e.target.y())}
        onTransform={(e) => onTransform(e.target)}
        onTransformEnd={onTransformEnd}
      />
      {shouldShowLabel(w, h) && (
        <Text
          text={mainLabel}
          x={x + 6}
          y={y + 4}
          fontSize={fontSizeFor()}
          fill="#111"
          listening={false}
          shadowColor="#fff"
          shadowBlur={3}
          shadowOpacity={1}
        />
      )}
    </>
  );
};
