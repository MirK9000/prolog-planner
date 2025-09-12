// apps/web/src/components/canvas/layers/ObjectsLayer.tsx
import React from 'react';
import { Rect, Text, Transformer } from 'react-konva';
import type { StaticObject, Rect as RectType } from '@planner/shared';
import { usePlanStore } from '../../../store/planStore';
import { TYPE_COLOR, TYPE_LABEL, TYPE_SHORT_LABEL, MIN_SIZE_MM } from '../config';

type Props = {
    view: { zoom: number };
    baseX: number;
    baseY: number;
    roomWpx: number;
    roomHpx: number;
    mm2px: number;
    ghost: RectType | null;
    ghostForbidden: boolean;
    doorZones: { id: string; z: RectType }[];
    handlers: {
        onDragMoveLive: (o: StaticObject, node: any) => void;
        onTransformLive: (o: StaticObject, node: any) => void;
        onDragEnd: (o: StaticObject, xPx: number, yPx: number) => void;
        onTransformEnd: (o: StaticObject, node: any) => void;
    };
};

export const ObjectsLayer: React.FC<Props> = ({ view, baseX, baseY, roomWpx, roomHpx, mm2px, ghost, ghostForbidden, doorZones, handlers }) => {
    const { plan, selectedId, setSelected } = usePlanStore();
    const [trRef, setTrRef] = React.useState<any>(null);
    const shapeRefs = React.useRef<Record<string, any>>({});

    React.useEffect(() => {
        if (trRef && selectedId && shapeRefs.current[selectedId]) {
            trRef.nodes([shapeRefs.current[selectedId]]);
        } else {
            trRef?.nodes([]);
        }
        trRef?.getLayer()?.batchDraw();
    }, [selectedId, trRef]);

    const MIN_PX = MIN_SIZE_MM * mm2px;
    const trBoundBox = (oldBox: any, newBox: any) => {
        let w = Math.max(newBox.width, MIN_PX), h = Math.max(newBox.height, MIN_PX);
        let x = newBox.x, y = newBox.y;
        const maxX = baseX + roomWpx - w, maxY = baseY + roomHpx - h;
        x = Math.min(Math.max(x, baseX), maxX); y = Math.min(Math.max(y, baseY), maxY);
        return { ...newBox, x, y, width: w, height: h };
    };

    const fontSizeFor = (selected: boolean) => selected ? 12 : Math.max(9, Math.min(12, 9 + (view.zoom - 1) * 3));

    return (
        <>
            {doorZones.map(({ id, z }) => (
                <Rect key={'dz_' + id} x={baseX + z.X * mm2px} y={baseY + z.Y * mm2px} width={z.W * mm2px} height={z.H * mm2px} fill="rgba(239,68,68,0.12)" stroke="#ef4444" dash={[6, 4]} listening={false} />
            ))}
            
            {plan.objects.map(o => {
                const { X, Y, W, H } = o.rect;
                const selected = selectedId === o.id;
                const style = TYPE_COLOR[o.type] || { fill: '#fafafa', stroke: '#6b7280' };
                const wPx = W * mm2px, hPx = H * mm2px;
                const showLabel = selected || (view.zoom > 0.45 && wPx > 50 && hPx > 26);
                const label = selected ? `${TYPE_LABEL[o.type] ?? o.type} · ${o.id}` : (TYPE_SHORT_LABEL[o.type] ?? o.type);
                
                return (
                    <React.Fragment key={o.id}>
                        <Rect ref={node => { if (node) shapeRefs.current[o.id] = node; }}
                            x={baseX + X * mm2px} y={baseY + Y * mm2px} width={wPx} height={hPx}
                            fill={style.fill} stroke={selected ? '#6366f1' : style.stroke} strokeWidth={selected ? 2 : 1}
                            draggable onClick={() => setSelected(o.id)} onDblClick={() => setSelected(undefined)}
                            onDragMove={e => handlers.onDragMoveLive(o, e.target)}
                            onTransform={e => handlers.onTransformLive(o, e.target)}
                            onDragEnd={e => handlers.onDragEnd(o, e.target.x(), e.target.y())}
                            onTransformEnd={e => handlers.onTransformEnd(o, e.target)}
                        />
                        {showLabel && <Text text={label} x={baseX + X * mm2px + 6} y={baseY + Y * mm2px + 4} fontSize={fontSizeFor(selected)} fill="#111" listening={false} />}
                    </React.Fragment>
                );
            })}

            <Transformer ref={setTrRef as any} rotateEnabled={false} ignoreStroke anchorSize={8} boundBoxFunc={trBoundBox} />

            {ghost && <Rect x={baseX + ghost.X * mm2px} y={baseY + ghost.Y * mm2px} width={ghost.W * mm2px} height={ghost.H * mm2px} fill={ghostForbidden ? 'rgba(239,68,68,0.25)' : 'rgba(34,197,94,0.2)'} stroke={ghostForbidden ? '#ef4444' : '#16a34a'} strokeWidth={2} dash={[6, 4]} listening={false} />}
        </>
    );
};
