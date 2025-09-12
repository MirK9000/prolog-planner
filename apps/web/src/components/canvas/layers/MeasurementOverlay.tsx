// apps/web/src/components/canvas/layers/MeasurementOverlay.tsx
import React from 'react';
import { Rect, Text, Line } from 'react-konva';
import type { Plan } from '@planner/shared';

type Props = {
    liveBox: { id: string; X: number; Y: number; W: number; H: number } | null;
    plan: Plan;
    selectedId?: string;
    baseX: number;
    baseY: number;
    mm2px: number;
    roomHpx: number;
};

export const MeasurementOverlay: React.FC<Props> = ({ liveBox, plan, selectedId, baseX, baseY, mm2px, roomHpx }) => {
    const selId = selectedId;
    if (!selId) return null;

    const srcObj = plan.objects.find((o) => o.id === selId);
    const src = liveBox && liveBox.id === selId ? liveBox : srcObj?.rect;
    if (!src) return null;
    
    const { X, Y, W, H } = src;

    const x = baseX + X * mm2px;
    const y = baseY + Y * mm2px;
    const w = W * mm2px;
    const h = H * mm2px;

    const widthLabel = `${Math.round(W)} мм`;
    const heightLabel = `${Math.round(H)} мм`;
    const posLabel = `X=${Math.round(X)} · Y=${Math.round(Y)}`;

    const off = 10, tick = 6, labelH = 16, labelW = 56, posBadgeH = 18;

    const canPlaceBottom = y + h + off + labelH + 2 <= baseY + roomHpx;
    const hLineY = canPlaceBottom ? y + h + off : y - off;
    const widthLabelY = canPlaceBottom ? hLineY + 2 : hLineY - labelH - 2;

    const canPlaceLeft = x - off - labelW - 4 >= baseX;
    const vLineX = canPlaceLeft ? x - off : x + w + off;
    const heightLabelX = canPlaceLeft ? vLineX - labelW - 4 : vLineX + 4;
    const heightAlign: 'left' | 'right' = canPlaceLeft ? 'right' : 'left';

    const posBadgeY = y - 22 < baseY + 2 ? y + 4 : y - 22;

    return (
        <>
            <Rect x={x} y={y} width={w} height={h} stroke="#111" dash={[4, 4]} strokeWidth={1} />
            <Line points={[x, hLineY, x + w, hLineY]} stroke="#111" />
            <Line points={[x, hLineY - tick / 2, x, hLineY + tick / 2]} stroke="#111" />
            <Line points={[x + w, hLineY - tick / 2, x + w, hLineY + tick / 2]} stroke="#111" />
            <Text text={widthLabel} x={x} y={widthLabelY} width={w} align="center" fontSize={12} fontStyle="600" fill="#111" />
            <Line points={[vLineX, y, vLineX, y + h]} stroke="#111" />
            <Line points={[vLineX - tick / 2, y, vLineX + tick / 2, y]} stroke="#111" />
            <Line points={[vLineX - tick / 2, y + h, vLineX + tick / 2, y + h]} stroke="#111" />
            <Text text={heightLabel} x={heightLabelX} y={y + h / 2 - 8} width={labelW} align={heightAlign} fontSize={12} fontStyle="600" fill="#111" />
            <Rect x={x} y={posBadgeY} width={140} height={posBadgeH} fill="rgba(255,255,255,0.9)" cornerRadius={4} stroke="#ddd" />
            <Text text={posLabel} x={x + 6} y={posBadgeY + 2} fontSize={12} fill="#111" />
        </>
    );
};

