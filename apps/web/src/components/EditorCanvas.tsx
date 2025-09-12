// apps/web/src/components/EditorCanvas.tsx
'use client';
import React from 'react';
import { Stage, Layer } from 'react-konva';
import { usePlanStore } from '../store/planStore';
import { useCanvasControls } from './canvas/hooks/useCanvasControls';
import { useObjectInteractions } from './canvas/hooks/useObjectInteractions';
import { GridLayer } from './canvas/layers/GridLayer';
import { ObjectsLayer } from './canvas/layers/ObjectsLayer';
import { MeasurementOverlay } from './canvas/layers/MeasurementOverlay';
import { PADDING_PX } from './canvas/config';
import type { Stage as StageType } from 'konva/lib/Stage';

const useContainerSize = (ref: React.RefObject<HTMLDivElement | null>) => {
    const [size, setSize] = React.useState({ w: 800, h: 600 });
    React.useLayoutEffect(() => {
        const el = ref.current;
        if (!el) return;
        const obs = new ResizeObserver(() => setSize({ w: el.clientWidth, h: el.clientHeight }));
        obs.observe(el);
        setSize({ w: el.clientWidth, h: el.clientHeight });
        return () => obs.disconnect();
    }, [ref]);
    return size;
};

export const EditorCanvas: React.FC = () => {
    const containerRef = React.useRef<HTMLDivElement>(null);
    const stageRef = React.useRef<StageType>(null);
    const { plan, selectedId, setSelected } = usePlanStore();
    const size = useContainerSize(containerRef);

    const { view, mm2px, cursor, handlers: controlHandlers } = useCanvasControls(stageRef, size, plan.room);
    
    const baseX = PADDING_PX + view.panX;
    const baseY = PADDING_PX + view.panY;

    const { ghost, liveBox, ghostForbidden, doorZonesMm, objectEventHandlers, stageEventHandlers } = useObjectInteractions(stageRef, baseX, baseY, mm2px);
    
    const stageHandlers = {
        onWheel: controlHandlers.onWheel,
        onMouseDown: (e: any) => { controlHandlers.onMouseDown(e); if (e.target === e.currentTarget) setSelected(undefined); },
        onMouseMove: (e: any) => { controlHandlers.onMouseMove(e); stageEventHandlers.onMouseMove(); },
        onMouseUp: controlHandlers.onMouseUp,
        onMouseLeave: controlHandlers.onMouseLeave,
        onClick: (e: any) => { if (e.target.getType() === 'Stage') stageEventHandlers.onClick(); },
        onContextMenu: (e:any) => e.evt.preventDefault(),
    };

    return (
        <div ref={containerRef} style={{ width: '100%', height: '100%', border: '1px solid #e5e7eb', borderRadius: 8, boxSizing: 'border-box', overflow: 'hidden' }}>
            <Stage ref={stageRef} width={size.w} height={size.h} style={{ cursor }} {...stageHandlers}>
                <Layer>
                    <GridLayer room={plan.room} baseX={baseX} baseY={baseY} mm2px={mm2px} />
                </Layer>
                <Layer>
                    <ObjectsLayer 
                        view={view} 
                        baseX={baseX} 
                        baseY={baseY}
                        roomWpx={plan.room.W * mm2px}
                        roomHpx={plan.room.H * mm2px}
                        mm2px={mm2px} 
                        ghost={ghost}
                        ghostForbidden={ghostForbidden}
                        doorZones={doorZonesMm}
                        handlers={objectEventHandlers}
                    />
                </Layer>
                <Layer listening={false}>
                    <MeasurementOverlay 
                        liveBox={liveBox} 
                        plan={plan}
                        selectedId={selectedId}
                        baseX={baseX} 
                        baseY={baseY} 
                        mm2px={mm2px}
                        roomHpx={plan.room.H * mm2px}
                    />
                </Layer>
            </Stage>
        </div>
    );
};

