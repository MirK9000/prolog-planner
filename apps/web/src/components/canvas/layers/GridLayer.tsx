// apps/web/src/components/canvas/layers/GridLayer.tsx
import React from 'react';
import { Line, Rect } from 'react-konva';
import { GRID_MM } from '../config';

type Props = {
    room: { W: number; H: number };
    baseX: number;
    baseY: number;
    mm2px: number;
};

export const GridLayer: React.FC<Props> = React.memo(({ room, baseX, baseY, mm2px }) => {
    const roomWpx = room.W * mm2px;
    const roomHpx = room.H * mm2px;

    const lines = React.useMemo(() => {
        const lines = [];
        for (let mm = 0; mm <= room.W; mm += GRID_MM) {
            const x = baseX + mm * mm2px;
            lines.push(<Line key={'v' + mm} points={[x, baseY, x, baseY + roomHpx]} stroke="#f0f0f0" strokeWidth={1} />);
        }
        for (let mm = 0; mm <= room.H; mm += GRID_MM) {
            const y = baseY + mm * mm2px;
            lines.push(<Line key={'h' + mm} points={[baseX, y, baseX + roomWpx, y]} stroke="#f0f0f0" strokeWidth={1} />);
        }
        return lines;
    }, [room.W, room.H, baseX, baseY, mm2px, roomWpx, roomHpx]);

    return (
        <>
            {lines}
            <Rect x={baseX} y={baseY} width={roomWpx} height={roomHpx} stroke="#ccc" strokeWidth={1} listening={false} />
        </>
    );
});