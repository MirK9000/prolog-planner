import React from 'react';
import { Rect, Line, Text } from 'react-konva';
import type { StaticObject } from '@planner/shared';

interface Props {
  selectedId?: string;
  liveBox: { id: string; X: number; Y: number; W: number; H: number } | null;
  plan: { objects: StaticObject[] };
  baseX: number;
  baseY: number;
  mm2px: number;
  roomWpx: number;
  roomHpx: number;
}

export const MeasurementOverlay: React.FC<Props> = ({
  selectedId,
  liveBox,
  plan,
  baseX,
  baseY,
  mm2px,
  roomWpx,
  roomHpx,
}) => {
  const selId = selectedId;
  if (!selId) return null;

  const src =
    liveBox && liveBox.id === selId
      ? liveBox
      : plan.objects.find((o) => o.id === selId);
  if (!src) return null;

  const hasRect = (src as any).rect !== undefined;
  const X = hasRect ? (src as any).rect.X : (src as any).X;
  const Y = hasRect ? (src as any).rect.Y : (src as any).Y;
  const W = hasRect ? (src as any).rect.W : (src as any).W;
  const H = hasRect ? (src as any).rect.H : (src as any).H;

  const x = baseX + X * mm2px;
  const y = baseY + Y * mm2px;
  const w = W * mm2px;
  const h = H * mm2px;

  const widthLabel = `${Math.round(W)} мм`;
  const heightLabel = `${Math.round(H)} мм`;
  const posLabel = `X=${Math.round(X)} мм · Y=${Math.round(Y)} мм`;

  const off = 10;
  const tick = 6;

  // ширина — снизу, если влезает; иначе сверху
  const labelH = 16;
  const canPlaceBottom = y + h + off + labelH + 2 <= baseY + roomHpx;
  const hLineY = canPlaceBottom ? y + h + off : y - off;
  const widthLabelY = canPlaceBottom ? hLineY + 2 : hLineY - labelH - 2;

  // высота — слева, если влезает; иначе справа
  const labelW = 56;
  const canPlaceLeft = x - off - labelW - 4 >= baseX;
  const vLineX = canPlaceLeft ? x - off : x + w + off;
  const heightLabelX = canPlaceLeft ? vLineX - labelW - 4 : vLineX + 4;
  const heightAlign: 'left' | 'right' = canPlaceLeft ? 'right' : 'left';

  // бейдж позиции — смещаем внутрь, если упирается вверх
  const posBadgeH = 18;
  const posBadgeY = y - 22 < baseY + 2 ? y + 4 : y - 22;

  return (
    <>
      {/* пунктирная рамка */}
      <Rect
        x={x}
        y={y}
        width={w}
        height={h}
        stroke="#111"
        dash={[4, 4]}
        strokeWidth={1}
        listening={false}
      />

      {/* горизонтальная линейка ширины */}
      <Line points={[x, hLineY, x + w, hLineY]} stroke="#111" listening={false} />
      <Line
        points={[x, hLineY - tick / 2, x, hLineY + tick / 2]}
        stroke="#111"
        listening={false}
      />
      <Line
        points={[x + w, hLineY - tick / 2, x + w, hLineY + tick / 2]}
        stroke="#111"
        listening={false}
      />
      <Text
        text={widthLabel}
        x={x}
        y={widthLabelY}
        width={w}
        align="center"
        fontSize={12}
        fontStyle="600"
        fill="#111"
        listening={false}
        shadowColor="#fff"
        shadowBlur={3}
        shadowOpacity={1}
      />

      {/* вертикальная линейка высоты */}
      <Line points={[vLineX, y, vLineX, y + h]} stroke="#111" listening={false} />
      <Line
        points={[vLineX - tick / 2, y, vLineX + tick / 2, y]}
        stroke="#111"
        listening={false}
      />
      <Line
        points={[vLineX - tick / 2, y + h, vLineX + tick / 2, y + h]}
        stroke="#111"
        listening={false}
      />
      <Text
        text={heightLabel}
        x={heightLabelX}
        y={y + h / 2 - 8}
        width={labelW}
        align={heightAlign}
        fontSize={12}
        fontStyle="600"
        fill="#111"
        listening={false}
        shadowColor="#fff"
        shadowBlur={3}
        shadowOpacity={1}
      />

      {/* Бейдж X/Y */}
      <Rect
        x={x}
        y={posBadgeY}
        width={140}
        height={posBadgeH}
        fill="rgba(255,255,255,0.9)"
        cornerRadius={4}
        stroke="#ddd"
        listening={false}
      />
      <Text
        text={posLabel}
        x={x + 6}
        y={posBadgeY + 2}
        fontSize={12}
        fill="#111"
        listening={false}
      />
    </>
  );
};
