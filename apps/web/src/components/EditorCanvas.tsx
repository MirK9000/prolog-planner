'use client';
import React from 'react';
import { Stage, Layer, Rect, Transformer, Line, Text } from 'react-konva';
import type { StaticObject } from '@planner/shared';
import { usePlanStore } from '../store/planStore';
import { clamp, snap } from '@planner/geometry';
import {
  PX_PER_MM,
  GRID_MM,
  MIN_SIZE_MM,
  PADDING_PX,
  TYPE_LABEL,
  TYPE_SHORT,
  TYPE_COLOR,
  DEFAULT_SIZE_MM,
  REQUIRES_WALL,
} from './canvasConstants';
import { rIntersects, computeDoorZone } from './doorZone';
import { useContainerSize } from '../hooks/useContainerSize';
import { MeasurementOverlay } from './MeasurementOverlay';

type View = { zoom: number; panX: number; panY: number };

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
  const spacePressed = React.useRef(false);

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
      const sx = stage?.width() / 2 ?? size.w / 2;
      const sy = stage?.height() / 2 ?? size.h / 2;
      zoomAt(sx, sy, factor);
    },
    [size.w, size.h, zoomAt]
  );

  React.useEffect(() => {
    const onKeyDown = (e: KeyboardEvent) => {
      if (e.code === 'Space') {
        spacePressed.current = true;
      }
      if ((e.key === '+' || e.key === '=') && !e.metaKey && !e.ctrlKey) {
        e.preventDefault();
        zoomAtCenter(1.1);
      }
      if (e.key === '-' && !e.metaKey && !e.ctrlKey) {
        e.preventDefault();
        zoomAtCenter(1 / 1.1);
      }
      if (e.key === '0') {
        e.preventDefault();
        setView({ zoom: 1, panX: 0, panY: 0 });
      }
      if (e.key.toLowerCase() === 'f') {
        e.preventDefault();
        setView({ zoom: 1, panX: 0, panY: 0 });
      }
      if (e.key === 'Escape') {
        setPlacingType(undefined);
        setGhost(null);
      }
      if ((e.key === 'Delete' || e.key === 'Backspace') && !placingType) {
        e.preventDefault();
        deleteSelected();
      }
    };
    const onKeyUp = (e: KeyboardEvent) => {
      if (e.code === 'Space') spacePressed.current = false;
    };
    window.addEventListener('keydown', onKeyDown);
    window.addEventListener('keyup', onKeyUp);
    return () => {
      window.removeEventListener('keydown', onKeyDown);
      window.removeEventListener('keyup', onKeyUp);
    };
  }, [zoomAtCenter, setPlacingType, deleteSelected, placingType]);

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

  // ======= запретные зоны перед дверями (мм) =======
  const doorZonesMm = React.useMemo(() => {
    return plan.objects
      .filter((o) => o.type === 'door')
      .map((d) => ({ id: d.id, z: computeDoorZone(plan.room, d.rect) }))
      .filter((x) => !!x.z) as {
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
    const conflict = doorZonesMm.some(({ z }) => rIntersects(ghost, z));
    if (conflict) return;

    const id = nextIdForType(placingType); // короткий последовательный id
    const obj: StaticObject = {
      id,
      type: placingType,
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
    const hit = doorZonesMm.some(({ z }) => rIntersects(newRect, z));
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
    updateObject(o.id, {
      W: newWmm,
      H: newHmm,
      X: Math.round((node.x() - baseX) / mm2px),
      Y: Math.round((node.y() - baseY) / mm2px),
    });
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

  // сетка
  const Grid: React.FC = () => {
    const lines = [];
    for (let mm = 0; mm <= plan.room.W; mm += GRID_MM) {
      const x = baseX + mm * mm2px;
      lines.push(
        <Line
          key={'v' + mm}
          points={[x, baseY, x, baseY + roomHpx]}
          stroke="#eee"
        />
      );
    }
    for (let mm = 0; mm <= plan.room.H; mm += GRID_MM) {
      const y = baseY + mm * mm2px;
      lines.push(
        <Line
          key={'h' + mm}
          points={[baseX, y, baseX + roomWpx, y]}
          stroke="#eee"
        />
      );
    }
    return <>{lines}</>;
  };

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
      return { ...newBox, x, y, width: w, height: h };
    },
    [MIN_PX, baseX, baseY, roomWpx, roomHpx]
  );

  // ======= Измерительный оверлей для выделенного объекта =======

  // ——— Правила отображения подписи внутри объекта ———
  const shouldShowLabel = (selected: boolean, w: number, h: number) => {
    if (selected) return true; // всегда для выделенного
    if (view.zoom < 0.45) return false; // слишком далеко — не показывать
    return w >= 50 && h >= 26; // объект достаточно крупный в px
  };
  const fontSizeFor = (selected: boolean) =>
    selected ? 12 : Math.max(9, Math.min(12, 9 + (view.zoom - 1) * 3));

  // «призрак» нельзя ставить в запретную зону
  const ghostForbidden =
    !!ghost && doorZonesMm.some(({ z }) => rIntersects(ghost, z));

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
          <Grid />
          {/* Комната */}
          <Rect
            x={baseX}
            y={baseY}
            width={roomWpx}
            height={roomHpx}
            stroke="#999"
          />

          {/* Запретные зоны перед дверями */}
          {doorZonesMm.map(({ id, z }) => (
            <Rect
              key={'dz_' + id}
              x={baseX + z.X * mm2px}
              y={baseY + z.Y * mm2px}
              width={z.W * mm2px}
              height={z.H * mm2px}
              fill="rgba(239,68,68,0.12)"
              stroke="#ef4444"
              dash={[6, 4]}
              listening={false}
            />
          ))}

          {/* Объекты */}
          {plan.objects.map((o) => {
            const x = baseX + o.rect.X * mm2px;
            const y = baseY + o.rect.Y * mm2px;
            const w = o.rect.W * mm2px;
            const h = o.rect.H * mm2px;

            const style = TYPE_COLOR[o.type] || {
              fill: 'rgba(99,102,241,0.08)',
              stroke: '#6b7280',
            };
            const selected = selectedId === o.id;
            const stroke = selected ? '#6366f1' : style.stroke;
            const strokeWidth = selected ? 2 : 1;
            const fill = style.fill;

            const mainLabel = selected
              ? `${TYPE_LABEL[o.type] ?? o.type} · ${o.id}`
              : TYPE_SHORT[o.type] ?? TYPE_LABEL[o.type] ?? o.type;

            return (
              <React.Fragment key={o.id}>
                <Rect
                  ref={(ref) => {
                    if (ref) shapeRefs.current[o.id] = ref;
                  }}
                  x={x}
                  y={y}
                  width={w}
                  height={h}
                  fill={fill}
                  stroke={stroke}
                  strokeWidth={strokeWidth}
                  draggable
                  dragBoundFunc={(pos) => {
                    const nx = clamp(pos.x, baseX, baseX + roomWpx - w);
                    const ny = clamp(pos.y, baseY, baseY + roomHpx - h);
                    return { x: nx, y: ny };
                  }}
                  onClick={() => setSelected(o.id)}
                  onDragMove={(e) => onDragMoveLive(o, e.target)}
                  onDragEnd={(e) =>
                    onDragEnd(o, e.target.x(), e.target.y())
                  }
                  onTransform={(e) => onTransformLive(o, e.target)}
                  onTransformEnd={() => onTransformEnd(o)}
                />

                {/* компактная подпись внутри фигуры */}
                {shouldShowLabel(selected, w, h) && (
                  <Text
                    text={mainLabel}
                    x={x + 6}
                    y={y + 4}
                    fontSize={fontSizeFor(selected)}
                    fill="#111"
                    listening={false}
                    shadowColor="#fff"
                    shadowBlur={3}
                    shadowOpacity={1}
                  />
                )}
              </React.Fragment>
            );
          })}

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

          {/* Призрак размещаемого объекта */}
          {placingType && ghost && (
            <Rect
              x={baseX + ghost.X * mm2px}
              y={baseY + ghost.Y * mm2px}
              width={ghost.W * mm2px}
              height={ghost.H * mm2px}
              fill={ghostForbidden ? 'rgba(239,68,68,0.25)' : 'rgba(34,197,94,0.2)'}
              stroke={ghostForbidden ? '#ef4444' : '#16a34a'}
              strokeWidth={2}
              dash={[6, 4]}
              listening={false}
            />
          )}
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
