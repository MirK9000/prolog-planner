// apps/web/src/components/canvas/hooks/useCanvasControls.ts
import React from 'react';
import { clamp } from '@planner/geometry';
import { usePlanStore } from '../../../store/planStore';
import { PADDING_PX, PX_PER_MM } from '../config';
import type { Stage } from 'konva/lib/Stage';

export type View = { zoom: number; panX: number; panY: number };

export function useCanvasControls(
    stageRef: React.RefObject<Stage | null>,
    size: { w: number; h: number },
    roomSize: { W: number; H: number }
) {
    const { deleteSelected, setPlacingType, placingType } = usePlanStore();
    const [view, setView] = React.useState<View>({ zoom: 1, panX: 0, panY: 0 });
    const [isPanning, setIsPanning] = React.useState(false);
    const panStart = React.useRef<{ x: number; y: number; panX: number; panY: number } | null>(null);
    const spacePressed = React.useRef(false);

    const fit = React.useMemo(() => {
        const roomPxBase = { W: roomSize.W * PX_PER_MM, H: roomSize.H * PX_PER_MM };
        const s = Math.min(size.w / (roomPxBase.W + PADDING_PX * 2), size.h / (roomPxBase.H + PADDING_PX * 2)) || 1;
        return Math.max(1e-6, Math.floor(s * 1000) / 1000);
    }, [size.w, size.h, roomSize.W, roomSize.H]);

    const mm2px = PX_PER_MM * fit * view.zoom;

    const zoomAt = React.useCallback((sx: number, sy: number, factor: number) => {
        setView(prev => {
            const old = prev.zoom;
            const next = clamp(old * factor, 0.2, 10);
            if (next === old) return prev;
            const oldMm2px = PX_PER_MM * fit * old;
            const newMm2px = PX_PER_MM * fit * next;
            const newPanX = (sx - PADDING_PX) - ((sx - PADDING_PX - prev.panX) / oldMm2px) * newMm2px;
            const newPanY = (sy - PADDING_PX) - ((sy - PADDING_PX - prev.panY) / oldMm2px) * newMm2px;
            return { zoom: next, panX: newPanX, panY: newPanY };
        });
    }, [fit]);

    const zoomAtCenter = React.useCallback((factor: number) => {
        const stage = stageRef.current;
        const sx = stage?.width() ?? size.w / 2;
        const sy = stage?.height() ?? size.h / 2;
        zoomAt(sx, sy, factor);
    }, [size.w, size.h, zoomAt, stageRef]);

    React.useEffect(() => {
        const onKeyDown = (e: KeyboardEvent) => {
            if (e.repeat) return;
            if (e.code === 'Space') { e.preventDefault(); spacePressed.current = true; }
            if ((e.key === '+' || e.key === '=') && !e.metaKey && !e.ctrlKey) { e.preventDefault(); zoomAtCenter(1.1); }
            if (e.key === '-' && !e.metaKey && !e.ctrlKey) { e.preventDefault(); zoomAtCenter(1 / 1.1); }
            if (e.key === '0' || e.key.toLowerCase() === 'f') { e.preventDefault(); setView({ zoom: 1, panX: 0, panY: 0 }); }
            if (e.key === 'Escape') setPlacingType(undefined);
            if ((e.key === 'Delete' || e.key === 'Backspace') && !placingType) deleteSelected();
        };
        const onKeyUp = (e: KeyboardEvent) => { if (e.code === 'Space') spacePressed.current = false; };
        window.addEventListener('keydown', onKeyDown);
        window.addEventListener('keyup', onKeyUp);
        return () => { window.removeEventListener('keydown', onKeyDown); window.removeEventListener('keyup', onKeyUp); };
    }, [zoomAtCenter, setPlacingType, deleteSelected, placingType]);

    const stopPan = () => {
        if (isPanning) {
            setIsPanning(false);
        }
    };

    React.useEffect(() => {
        window.addEventListener('mouseup', stopPan);
        window.addEventListener('blur', stopPan);
        return () => { window.removeEventListener('mouseup', stopPan); window.removeEventListener('blur', stopPan); };
    }, []);

    const handleWheel = (e: any) => {
        e.evt.preventDefault();
        const stage = stageRef.current;
        const p = stage?.getPointerPosition();
        if (!p) return;
        const dir = e.evt.deltaY > 0 ? (1 / 1.1) : 1.1;
        zoomAt(p.x, p.y, dir);
    };

    const handleMouseDown = (e: any) => {
        if (placingType) return;
        if (spacePressed.current || e.evt.button === 1) {
            setIsPanning(true);
            panStart.current = { x: e.evt.clientX, y: e.evt.clientY, panX: view.panX, panY: view.panY };
        }
    };
    
    const handleMouseMove = (e: any) => {
        if (!isPanning || !panStart.current) return;
        if (e.evt.buttons === 0) { setIsPanning(false); return; }
        const dx = e.evt.clientX - panStart.current.x;
        const dy = e.evt.clientY - panStart.current.y;
        setView(v => ({ ...v, panX: panStart.current!.panX + dx, panY: panStart.current!.panY + dy }));
    };

    const cursor = placingType ? 'copy' : (isPanning || spacePressed.current ? (isPanning ? 'grabbing' : 'grab') : 'default');

    return { view, mm2px, cursor, handlers: { onWheel: handleWheel, onMouseDown: handleMouseDown, onMouseMove: handleMouseMove, onMouseUp: stopPan, onMouseLeave: stopPan } };
}

