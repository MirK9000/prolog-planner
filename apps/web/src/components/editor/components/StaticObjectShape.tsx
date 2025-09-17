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
  const hasOrientation = object.type === 'workplace' && object.orientation !== undefined;
  const orientation = hasOrientation ? object.orientation! : undefined;
  const strokeWidth = selected ? 2 : 1;
  const containerStroke = hasOrientation
    ? selected
      ? '#6366f1'
      : '#9ca3af'
    : selected
    ? '#6366f1'
    : style.stroke;
  const containerFill = hasOrientation ? 'transparent' : style.fill;
  const containerDash = hasOrientation ? [6, 4] : undefined;
  const tableStroke = selected ? '#6366f1' : style.stroke;
  const tableStrokeWidth = selected ? 2 : 1;
  const tableFill = style.fill;

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

  let tableRectPx: { x: number; y: number; width: number; height: number } | null = null;
  if (hasOrientation && orientation !== undefined) {
    const WALKWAY_MM = 600;
    const baseRect = object.rect;
    let tableXmm = baseRect.X;
    let tableYmm = baseRect.Y;
    let tableWmm = baseRect.W;
    let tableHmm = baseRect.H;

    if (orientation === 0) {
      const walkway = Math.min(WALKWAY_MM, tableHmm);
      tableYmm = baseRect.Y + walkway;
      tableHmm = Math.max(0, tableHmm - walkway);
    } else if (orientation === 1) {
      const walkway = Math.min(WALKWAY_MM, tableWmm);
      tableXmm = baseRect.X + walkway;
      tableWmm = Math.max(0, tableWmm - walkway);
    } else if (orientation === 2) {
      const walkway = Math.min(WALKWAY_MM, tableHmm);
      tableHmm = Math.max(0, tableHmm - walkway);
    } else if (orientation === 3) {
      const walkway = Math.min(WALKWAY_MM, tableWmm);
      tableWmm = Math.max(0, tableWmm - walkway);
    }

    tableRectPx = {
      x: baseX + tableXmm * mm2px,
      y: baseY + tableYmm * mm2px,
      width: tableWmm * mm2px,
      height: tableHmm * mm2px,
    };
  }

  return (
    <>
      <Rect
        ref={setRef}
        x={x}
        y={y}
        width={w}
        height={h}
        fill={containerFill}
        stroke={containerStroke}
        strokeWidth={strokeWidth}
        dash={containerDash}
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
      {tableRectPx && (
        <Rect
          x={tableRectPx.x}
          y={tableRectPx.y}
          width={tableRectPx.width}
          height={tableRectPx.height}
          fill={tableFill}
          stroke={tableStroke}
          strokeWidth={tableStrokeWidth}
          listening={false}
        />
      )}
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
