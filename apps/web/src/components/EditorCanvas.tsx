'use client';
import React from 'react';
import { Stage, Layer, Transformer } from 'react-konva';
import type { StaticObject } from '@planner/shared';
import { usePlanStore } from '../store/planStore';
import { clamp, snap } from '@planner/geometry';
import {
  PX_PER_MM,
  GRID_MM,
  MIN_SIZE_MM,
  PADDING_PX,
  DEFAULT_SIZE_MM,
  REQUIRES_WALL,
} from './editor/config';
import { rIntersects, computeDoorZone, computeEquipmentZone } from './editor/doorZone';
import { useContainerSize, useEditorHotkeys, View } from './editor/hooks';
import {
  Grid,
  Room,
  ForbiddenZones,
  StaticObjectShape,
  PlacementGhost,
  MeasurementOverlay,
} from './editor/components';


export const EditorCanvas: React.FC = () => {
  const containerRef = React.useRef<HTMLDivElement>(null);
  const stageRef = React.useRef<any>(null);
  const size = useContainerSize(containerRef);

  const {
    plan,
    updateObject,
    selectedId,
    setSelected,
    placingType,
    setPlacingType,
    addObject,
    deleteSelected,
    nextIdForType,
  } = usePlanStore();

  // fit без учёта zoom
  const roomPxBase = { W: plan.room.W * PX_PER_MM, H: plan.room.H * PX_PER_MM };
  const fit = React.useMemo(() => {
    const s =
      Math.min(
        size.w / (roomPxBase.W + PADDING_PX * 2),
        size.h / (roomPxBase.H + PADDING_PX * 2)
      ) || 1;
    return Math.max(1e-6, Math.floor(s * 1000) / 1000);
  }, [size.w, size.h, roomPxBase.W, roomPxBase.H]);

  const [view, setView] = React.useState<View>({ zoom: 1, panX: 0, panY: 0 });
  const mm2px = PX_PER_MM * fit * view.zoom;

  // панорамирование
  const [isPanning, setIsPanning] = React.useState(false);
  const panStart = React.useRef<{
    x: number;
    y: number;
    panX: number;
    panY: number;
  } | null>(null);

  // «живой» бокс и «призрак»
  const [liveBox, setLiveBox] = React.useState<{
    id: string;
    X: number;
    Y: number;
    W: number;
    H: number;
  } | null>(null);
  const [ghost, setGhost] = React.useState<{
    X: number;
    Y: number;
    W: number;
    H: number;
  } | null>(null);

  // ======= зум =======
  const zoomAt = React.useCallback(
    (sx: number, sy: number, factor: number) => {
      setView((prev) => {
        const old = prev.zoom;
        const next = clamp(old * factor, 0.2, 10);
        if (next === old) return prev;
        const baseX = PADDING_PX,
          baseY = PADDING_PX;
        const oldMm2px = PX_PER_MM * fit * old;
        const newMm2px = PX_PER_MM * fit * next;
        const newPanX =
          sx -
          baseX -
          ((sx - baseX - prev.panX) / oldMm2px) * newMm2px;
        const newPanY =
          sy -
          baseY -
          ((sy - baseY - prev.panY) / oldMm2px) * newMm2px;
        return { zoom: next, panX: newPanX, panY: newPanY };
      });
    },
    [fit]
  );

  const zoomAtCenter = React.useCallback(
    (factor: number) => {
      const stage = stageRef.current as any;
      const sx = (stage?.width() ?? size.w) / 2;
      const sy = (stage?.height() ?? size.h) / 2;
      zoomAt(sx, sy, factor);
    },
    [size.w, size.h, zoomAt]
  );

  const spacePressed = useEditorHotkeys(zoomAtCenter, setView, setGhost);

  // Завершать пан при mouseup/blur
  React.useEffect(() => {
    const up = () => stopPan();
    window.addEventListener('mouseup', up);
    window.addEventListener('blur', up);
    return () => {
      window.removeEventListener('mouseup', up);
      window.removeEventListener('blur', up);
    };
  }, []);

  const handleWheel = (e: any) => {
    e.evt.preventDefault();
    const stage = stageRef.current as any;
    const p = stage.getPointerPosition();
    if (!p) return;
    const dir = e.evt.deltaY > 0 ? 1 / 1.1 : 1.1;
    zoomAt(p.x, p.y, dir);
  };

  // Базовые вычисления
  const baseX = PADDING_PX + view.panX;
  const baseY = PADDING_PX + view.panY;
  const roomWpx = plan.room.W * mm2px;
  const roomHpx = plan.room.H * mm2px;

  // ======= запретные зоны (двери и оборудование) в мм =======
  const forbiddenZonesMm = React.useMemo(() => {
    const doorZones = plan.objects
      .filter((o) => o.type === 'door')
      .map((d) => ({ id: d.id, z: computeDoorZone(plan.room, d.rect) }))
      .filter((x) => !!x.z);
    const equipZones = plan.objects
      .filter((o) => o.type === 'electrical_shield' || o.type === 'net_cabinet')
      .map((e) => ({ id: e.id, z: computeEquipmentZone(plan.room, e.rect) }))
      .filter((x) => !!x.z);
    return [...doorZones, ...equipZones] as {
      id: string;
      z: { X: number; Y: number; W: number; H: number };
    }[];
  }, [plan]);

  // ======= размещение: «призрак» и добавление =======
  const updateGhostAt = () => {
    if (!placingType) return;
    const def = DEFAULT_SIZE_MM[placingType] ?? { W: 1000, H: 1000 };
    let W = def.W,
      H = def.H;
    const stage = stageRef.current as any;
    const pos = stage?.getPointerPosition();
    if (!pos) return;

    let X = Math.round((pos.x - baseX - (W * mm2px) / 2) / mm2px);
    let Y = Math.round((pos.y - baseY - (H * mm2px) / 2) / mm2px);
    X = snap(X, GRID_MM);
    Y = snap(Y, GRID_MM);

    if (REQUIRES_WALL.has(placingType)) {
      const dLeft = X,
        dTop = Y,
        dRight = plan.room.W - (X + W),
        dBottom = plan.room.H - (Y + H);
      const min = Math.min(dLeft, dTop, dRight, dBottom);
      if (min === dLeft) X = 0;
      else if (min === dTop) Y = 0;
      else if (min === dRight) X = plan.room.W - W;
      else Y = plan.room.H - H;
    }
    X = clamp(X, 0, plan.room.W - W);
    Y = clamp(Y, 0, plan.room.H - H);
    setGhost({ X, Y, W, H });
  };

  const handleMouseDown = (e: any) => {
    if (placingType) return;
    const btn = e.evt.button;
    if (spacePressed.current || btn === 1) {
      setIsPanning(true);
      panStart.current = {
        x: e.evt.clientX,
        y: e.evt.clientY,
        panX: view.panX,
        panY: view.panY,
      };
    }
  };
  const handleMouseMove = (e: any) => {
    if (placingType) {
      updateGhostAt();
      return;
    }
    if (!isPanning) return;
    const ps = panStart.current;
    if (!ps) return;
    if (e.evt.buttons === 0) {
      stopPan();
      return;
    }
    const dx = e.evt.clientX - ps.x,
      dy = e.evt.clientY - ps.y;
    setView((v) => ({ ...v, panX: ps.panX + dx, panY: ps.panY + dy }));
  };
  const handleClick = () => {
    if (!placingType || !ghost) return;
    // запретить размещение в зоне перед дверью
    const conflict = forbiddenZonesMm.some(({ z }) => rIntersects(ghost, z));
    if (conflict) return;

    const id = nextIdForType(placingType); // короткий последовательный id
    const obj: StaticObject = {
      id,
      type: placingType as StaticObject['type'],
      rect: { ...ghost },
      properties: [],
      requiresWallAnchor: REQUIRES_WALL.has(placingType),
    };
    addObject(obj);
    setSelected(id);
  };

  const stopPan = () => {
    setIsPanning(false);
    panStart.current = null;
  };

  // ==== drag/transform существующих объектов (liveBox -> overlay) ====
  const onDragMoveLive = (o: StaticObject, node: any) => {
    const X = Math.round((node.x() - baseX) / mm2px);
    const Y = Math.round((node.y() - baseY) / mm2px);
    setLiveBox({ id: o.id, X, Y, W: o.rect.W, H: o.rect.H });
  };
  const onTransformLive = (o: StaticObject, node: any) => {
    const scaleX = node.scaleX(),
      scaleY = node.scaleY();
    const W = Math.max(
      MIN_SIZE_MM,
      Math.round((node.width() * scaleX) / mm2px)
    );
    const H = Math.max(
      MIN_SIZE_MM,
      Math.round((node.height() * scaleY) / mm2px)
    );
    const X = Math.round((node.x() - baseX) / mm2px),
      Y = Math.round((node.y() - baseY) / mm2px);
    setLiveBox({ id: o.id, X, Y, W, H });
  };
  const onDragEnd = (o: StaticObject, xPx: number, yPx: number) => {
    const mmX = Math.round((xPx - baseX) / mm2px),
      mmY = Math.round((yPx - baseY) / mm2px);
    const snappedX = snap(mmX, GRID_MM),
      snappedY = snap(mmY, GRID_MM);

    const newRect = { X: snappedX, Y: snappedY, W: o.rect.W, H: o.rect.H };
    const hit = forbiddenZonesMm
      .filter(({ id }) => id !== o.id)
      .some(({ z }) => rIntersects(newRect, z));
    if (hit) {
      // откат: оставим на старых координатах
      updateObject(o.id, { X: o.rect.X, Y: o.rect.Y });
      setLiveBox(null);
      return;
    }

    if (o.requiresWallAnchor || o.type === 'window' || o.type === 'door') {
      const distLeft = snappedX;
      const distTop = snappedY;
      const distRight = plan.room.W - (snappedX + o.rect.W);
      const distBottom = plan.room.H - (snappedY + o.rect.H);
      const min = Math.min(distLeft, distTop, distRight, distBottom);
      if (min === distLeft) updateObject(o.id, { X: 0, Y: snappedY });
      else if (min === distTop) updateObject(o.id, { X: snappedX, Y: 0 });
      else if (min === distRight)
        updateObject(o.id, {
          X: plan.room.W - o.rect.W,
          Y: snappedY,
        });
      else updateObject(o.id, { X: snappedX, Y: plan.room.H - o.rect.H });
    } else {
    updateObject(o.id, { X: snappedX, Y: snappedY });
    }
    setLiveBox(null);
  };
  const onTransformEnd = (o: StaticObject) => {
    const node = shapeRefs.current[o.id];
    const scaleX = node.scaleX(),
      scaleY = node.scaleY();
    let newWmm = Math.max(
      MIN_SIZE_MM,
      Math.round((node.width() * scaleX) / mm2px)
    );
    let newHmm = Math.max(
      MIN_SIZE_MM,
      Math.round((node.height() * scaleY) / mm2px)
    );
    newWmm = snap(newWmm, GRID_MM);
    newHmm = snap(newHmm, GRID_MM);
    node.scaleX(1);
    node.scaleY(1);
    const newRect = {
      W: newWmm,
      H: newHmm,
      X: Math.round((node.x() - baseX) / mm2px),
      Y: Math.round((node.y() - baseY) / mm2px),
    };
    const hit = forbiddenZonesMm
      .filter(({ id }) => id !== o.id)
      .some(({ z }) => rIntersects(newRect, z));
    if (hit) {
      node.x(baseX + o.rect.X * mm2px);
      node.y(baseY + o.rect.Y * mm2px);
      node.width(o.rect.W * mm2px);
      node.height(o.rect.H * mm2px);
      updateObject(o.id, { ...o.rect });
      setLiveBox(null);
      return;
    }
    updateObject(o.id, newRect);
    setLiveBox(null);
  };

  const [trRef, setTrRef] = React.useState<any>(null);
  const shapeRefs = React.useRef<Record<string, any>>({});

  React.useEffect(() => {
    if (trRef && selectedId && shapeRefs.current[selectedId]) {
      trRef.nodes([shapeRefs.current[selectedId]]);
      trRef.getLayer()?.batchDraw();
    }
  }, [selectedId, trRef, view.zoom, view.panX, view.panY]);

  // ограничения трансформера
  const MIN_PX = MIN_SIZE_MM * mm2px;
  const trBoundBox = React.useCallback(
    (oldBox: any, newBox: any) => {
      let w = Math.max(newBox.width, MIN_PX),
        h = Math.max(newBox.height, MIN_PX);
      let x = newBox.x,
        y = newBox.y;
      const maxX = baseX + roomWpx - w,
        maxY = baseY + roomHpx - h;
      x = Math.min(Math.max(x, baseX), maxX);
      y = Math.min(Math.max(y, baseY), maxY);
      const rect = {
        X: Math.round((x - baseX) / mm2px),
        Y: Math.round((y - baseY) / mm2px),
        W: Math.round(w / mm2px),
        H: Math.round(h / mm2px),
      };
      const hit = forbiddenZonesMm
        .filter(({ id }) => id !== selectedId)
        .some(({ z }) => rIntersects(rect, z));
      if (hit) return oldBox;
      return { ...newBox, x, y, width: w, height: h };
    },
    [MIN_PX, baseX, baseY, roomWpx, roomHpx, mm2px, forbiddenZonesMm, selectedId]
  );


  // «призрак» нельзя ставить в запретную зону
  const ghostForbidden =
    !!ghost && forbiddenZonesMm.some(({ z }) => rIntersects(ghost, z));

  return (
    <div
      ref={containerRef}
      style={{
        width: '100%',
        height: '100%',
        position: 'relative',
        border: '1px solid #e5e7eb',
        borderRadius: 8,
        boxSizing: 'border-box',
        overflow: 'hidden',
      }}
    >
      <Stage
        ref={stageRef}
        width={Math.floor(size.w)}
        height={Math.floor(size.h)}
        onWheel={handleWheel}
        onMouseDown={handleMouseDown}
        onMouseMove={handleMouseMove}
        onClick={handleClick}
        onMouseUp={stopPan}
        onMouseLeave={stopPan}
        onContextMenu={(e: any) => e.evt.preventDefault()}
        style={{
          cursor: placingType
            ? 'copy'
            : isPanning || spacePressed.current
            ? isPanning
              ? 'grabbing'
              : 'grab'
            : 'default',
        }}
      >
        <Layer>
          <Grid
            baseX={baseX}
            baseY={baseY}
            room={plan.room}
            mm2px={mm2px}
            grid={GRID_MM}
          />
          <Room baseX={baseX} baseY={baseY} roomWpx={roomWpx} roomHpx={roomHpx} />
          <ForbiddenZones zones={forbiddenZonesMm} baseX={baseX} baseY={baseY} mm2px={mm2px} />

          {plan.objects.map((o) => (
            <StaticObjectShape
              key={o.id}
              object={o}
              selected={selectedId === o.id}
              baseX={baseX}
              baseY={baseY}
              mm2px={mm2px}
              roomWpx={roomWpx}
              roomHpx={roomHpx}
              onClick={() => setSelected(o.id)}
              onDragMove={(node) => onDragMoveLive(o, node)}
              onDragEnd={(x, y) => onDragEnd(o, x, y)}
              onTransform={(node) => onTransformLive(o, node)}
              onTransformEnd={() => onTransformEnd(o)}
              setRef={(ref) => {
                if (ref) shapeRefs.current[o.id] = ref;
              }}
              zoom={view.zoom}
              forbiddenZones={forbiddenZonesMm.filter(({ id }) => id !== o.id)}
            />
          ))}

          <Transformer
            ref={setTrRef as any}
            rotateEnabled={false}
            ignoreStroke
            enabledAnchors={[
              'top-left',
              'top-center',
              'top-right',
              'middle-left',
              'middle-right',
              'bottom-left',
              'bottom-center',
              'bottom-right',
            ]}
            anchorSize={8}
            boundBoxFunc={trBoundBox}
          />

          <PlacementGhost
            ghost={placingType ? ghost : null}
            baseX={baseX}
            baseY={baseY}
            mm2px={mm2px}
            forbidden={ghostForbidden}
          />
        </Layer>

        {/* Верхний слой: измерительный оверлей */}
        <Layer listening={false}>
          <MeasurementOverlay
            selectedId={selectedId}
            liveBox={liveBox}
            plan={plan}
            baseX={baseX}
            baseY={baseY}
            mm2px={mm2px}
            roomWpx={roomWpx}
            roomHpx={roomHpx}
          />
        </Layer>
      </Stage>
    </div>
  );
};
