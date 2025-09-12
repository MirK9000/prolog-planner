// apps/web/src/components/canvas/hooks/useObjectInteractions.ts
import React from 'react';
import type { StaticObject, Rect as RectType, Size } from '@planner/shared';
import { usePlanStore } from '../../../store/planStore';
import { clamp, snap, rectIntersects } from '@planner/geometry';
import { DEFAULT_SIZE_MM, REQUIRES_WALL_ANCHOR, GRID_MM, MIN_SIZE_MM, computeDoorZone } from '../config';
import type { Stage } from 'konva/lib/Stage';

export function useObjectInteractions(
    stageRef: React.RefObject<Stage | null>,
    baseX: number,
    baseY: number,
    mm2px: number
) {
    const { plan, placingType, setPlacingType, addObject, nextIdForType, setSelected, updateObject } = usePlanStore();
    const [ghost, setGhost] = React.useState<RectType | null>(null);
    const [liveBox, setLiveBox] = React.useState<{ id: string; X: number; Y: number; W: number; H: number } | null>(null);

    const doorZonesMm = React.useMemo(() => {
        return plan.objects
          .filter((o) => o.type === 'door')
          .map((d) => ({ id: d.id, z: computeDoorZone(plan.room, d.rect) }))
          .filter((x) => !!x.z) as { id: string; z: RectType }[];
    }, [plan]);

    const updateGhostAt = React.useCallback(() => {
        if (!placingType) { if (ghost) setGhost(null); return; }
        const def = DEFAULT_SIZE_MM[placingType] ?? { W: 1000, H: 1000 };
        const { W, H } = def;
        const stage = stageRef.current;
        const pos = stage?.getPointerPosition();
        if (!pos) return;

        let X = Math.round((pos.x - baseX - (W * mm2px) / 2) / mm2px);
        let Y = Math.round((pos.y - baseY - (H * mm2px) / 2) / mm2px);
        X = snap(X, GRID_MM); Y = snap(Y, GRID_MM);

        if (REQUIRES_WALL_ANCHOR.has(placingType)) {
            const dLeft = X, dTop = Y, dRight = plan.room.W - (X + W), dBottom = plan.room.H - (Y + H);
            const min = Math.min(dLeft, dTop, dRight, dBottom);
            if (min === dLeft) X = 0; else if (min === dTop) Y = 0;
            else if (min === dRight) X = plan.room.W - W; else Y = plan.room.H - H;
        }
        X = clamp(X, 0, plan.room.W - W); Y = clamp(Y, 0, plan.room.H - H);
        setGhost({ X, Y, W, H });
    }, [placingType, stageRef, baseX, baseY, mm2px, plan.room.W, plan.room.H, ghost]);
    
    const handleClick = () => {
        if (!placingType || !ghost) return;
        const conflict = doorZonesMm.some(({ z }) => rectIntersects(ghost, z));
        if (conflict) return;

        const id = nextIdForType(placingType);
        const obj: StaticObject = {
            id, type: placingType as any, rect: ghost, properties: [],
            requiresWallAnchor: REQUIRES_WALL_ANCHOR.has(placingType),
        };
        addObject(obj);
        setSelected(id);
    };

    const handleMouseMove = () => {
        if (placingType) updateGhostAt();
    };

    const onDragMoveLive = (o: StaticObject, node: any) => {
        const X = Math.round((node.x() - baseX) / mm2px);
        const Y = Math.round((node.y() - baseY) / mm2px);
        setLiveBox({ id: o.id, X, Y, W: o.rect.W, H: o.rect.H });
    };
    
    const onTransformLive = (o: StaticObject, node: any) => {
        const scaleX = node.scaleX(), scaleY = node.scaleY();
        const W = Math.max(MIN_SIZE_MM, Math.round((node.width() * scaleX) / mm2px));
        const H = Math.max(MIN_SIZE_MM, Math.round((node.height() * scaleY) / mm2px));
        const X = Math.round((node.x() - baseX) / mm2px);
        const Y = Math.round((node.y() - baseY) / mm2px);
        setLiveBox({ id: o.id, X, Y, W, H });
    };

    const onDragEnd = (o: StaticObject, xPx: number, yPx: number) => {
        const mmX = Math.round((xPx - baseX) / mm2px);
        const mmY = Math.round((yPx - baseY) / mm2px);
        let snappedX = snap(mmX, GRID_MM);
        let snappedY = snap(mmY, GRID_MM);

        const newRect = { X: snappedX, Y: snappedY, W: o.rect.W, H: o.rect.H };
        const hit = doorZonesMm.some(({ z }) => rectIntersects(newRect, z));
        if (hit) {
            updateObject(o.id, { X: o.rect.X, Y: o.rect.Y });
            setLiveBox(null);
            return;
        }

        if (REQUIRES_WALL_ANCHOR.has(o.type)) {
            const { W, H } = o.rect;
            const dist = { l: snappedX, t: snappedY, r: plan.room.W - (snappedX + W), b: plan.room.H - (snappedY + H) };
            const min = Math.min(dist.l, dist.t, dist.r, dist.b);
            if (min === dist.l) snappedX = 0;
            else if (min === dist.t) snappedY = 0;
            else if (min === dist.r) snappedX = plan.room.W - W;
            else snappedY = plan.room.H - H;
        }
        updateObject(o.id, { X: snappedX, Y: snappedY });
        setLiveBox(null);
    };

    const onTransformEnd = (o: StaticObject, node: any) => {
        const scaleX = node.scaleX(), scaleY = node.scaleY();
        const newWmm = snap(Math.max(MIN_SIZE_MM, Math.round(node.width() * scaleX / mm2px)), GRID_MM);
        const newHmm = snap(Math.max(MIN_SIZE_MM, Math.round(node.height() * scaleY / mm2px)), GRID_MM);
        node.scaleX(1); node.scaleY(1);
        updateObject(o.id, { W: newWmm, H: newHmm, X: Math.round((node.x() - baseX) / mm2px), Y: Math.round((node.y() - baseY) / mm2px) });
        setLiveBox(null);
    };
    
    React.useEffect(() => {
        if (!placingType) setGhost(null);
    }, [placingType]);

    const ghostForbidden = !!ghost && doorZonesMm.some(({ z }) => rectIntersects(ghost, z));

    return { 
        ghost, 
        liveBox,
        ghostForbidden,
        doorZonesMm,
        objectEventHandlers: { onDragMoveLive, onTransformLive, onDragEnd, onTransformEnd },
        stageEventHandlers: { onClick: handleClick, onMouseMove: handleMouseMove } 
    };
}

