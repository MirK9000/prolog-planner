'use client';
import React from 'react';
import { Stage, Layer, Rect, Transformer, Line, Text } from 'react-konva';
import type { StaticObject } from '@planner/shared';
import { usePlanStore } from '../store/planStore';
import { clamp, snap } from '@planner/geometry';

const PX_PER_MM = 0.1;
const GRID_MM = 100;
const MIN_SIZE_MM = 100;
const PADDING_PX = 20;

// Человеко-читабельные названия (RU)
const TYPE_LABEL: Record<string, string> = {
  door: 'Дверь',
  window: 'Окно',
  column: 'Колонна',
  workplace: 'РМ',
  fire_extinguisher: 'Огнетуш.',
  fire_alarm: 'Пож. изв.',
  electrical_shield: 'Эл. щит',
  comms_block: 'Связь',
  net_cabinet: 'Сет. шкаф',
  cabinet: 'Шкаф',
  wall: 'Стена',
};

// Короткие подписи для мелких объектов
const TYPE_SHORT: Record<string, string> = {
  workplace: 'РМ',
  door: 'Дверь',
  window: 'Окно',
  column: 'Кол.',
  fire_extinguisher: 'Огн.',
  fire_alarm: 'Изв.',
  electrical_shield: 'Щит',
  comms_block: 'Связь',
  net_cabinet: 'Сет.',
  cabinet: 'Шкаф',
  wall: 'Ст.',
};

// Цвета
const TYPE_COLOR: Record<string, { fill: string; stroke: string }> = {
  door: { fill: '#f59e0b33', stroke: '#f59e0b' },
  window: { fill: '#38bdf833', stroke: '#38bdf8' },
  column: { fill: '#94a3b833', stroke: '#94a3b8' },
  workplace: { fill: '#22c55e33', stroke: '#22c55e' },
  fire_extinguisher: { fill: '#ef444433', stroke: '#ef4444' },
  fire_alarm: { fill: '#f9731633', stroke: '#f97316' },
  electrical_shield: { fill: '#06b6d433', stroke: '#06b6d4' },
  comms_block: { fill: '#e879f933', stroke: '#e879f9' },
  net_cabinet: { fill: '#10b98133', stroke: '#10b981' },
  cabinet: { fill: '#d946ef33', stroke: '#d946ef' },
  wall: { fill: '#9ca3af33', stroke: '#9ca3af' },
};

// размеры по умолчанию для новых объектов (мм)
const DEFAULT_SIZE_MM: Record<string, { W: number; H: number }> = {
  workplace: { W: 1200, H: 800 },
  door: { W: 900, H: 200 },
  window: { W: 2000, H: 200 },
  column: { W: 1000, H: 1000 },
  fire_extinguisher: { W: 200, H: 200 },
  fire_alarm: { W: 200, H: 200 },
  electrical_shield: { W: 800, H: 200 },
  cabinet: { W: 1000, H: 500 },
  net_cabinet: { W: 800, H: 600 },
  comms_block: { W: 400, H: 200 },
};

// какие типы «прибиваются» к стене
const REQUIRES_WALL = new Set(['door', 'window']);

// ===== Запретная зона перед дверью (СП 1.13130.2020 п.4.2.21) =====
const DOOR_CLEAR_FACTOR = 1.5; // квадрат со стороной 1.5*W двери, направлен внутрь
// пересечение прямоугольников (мм)
const rIntersects = (
  a: { X: number; Y: number; W: number; H: number },
  b: { X: number; Y: number; W: number; H: number }
) => a.X < b.X + b.W && a.X + a.W > b.X && a.Y < b.Y + b.H && a.Y + a.H > b.Y;

// клиентская версия вычисления зоны
const computeDoorZone = (
  room: { W: number; H: number },
  door: { X: number; Y: number; W: number; H: number }
) => {
  const { X, Y, W, H } = door;
  const side = Math.round(DOOR_CLEAR_FACTOR * W);
  const cx = X + W / 2;
  const cy = Y + H / 2;
  const clamp01 = (v: number, min: number, max: number) => Math.max(min, Math.min(max, v));

  if (X === 0) {
    // левая стена → зона вправо
    return {
      X: X + W,
      Y: clamp01(Math.round(cy - side / 2), 0, room.H - side),
      W: Math.min(side, room.W - (X + W)),
      H: Math.min(side, room.H),
    };
  }
  if (X + W === room.W) {
    // правая стена → зона влево
    return {
      X: Math.max(0, room.W - W - side),
      Y: clamp01(Math.round(cy - side / 2), 0, room.H - side),
      W: Math.min(side, room.W - W),
      H: Math.min(side, room.H),
    };
  }
  if (Y === 0) {
    // верхняя стена → зона вниз
    return {
      X: clamp01(Math.round(cx - side / 2), 0, room.W - side),
      Y: Y + H,
      W: Math.min(side, room.W),
      H: Math.min(side, room.H - (Y + H)),
    };
  }
  if (Y + H === room.H) {
    // нижняя стена → зона вверх
    return {
      X: clamp01(Math.round(cx - side / 2), 0, room.W - side),
      Y: Math.max(0, room.H - H - side),
      W: Math.min(side, room.W),
      H: Math.min(side, room.H - H),
    };
  }
  return null;
};
// ================================================================

type View = { zoom: number; panX: number; panY: number };

const useContainerSize = (ref: React.RefObject<HTMLDivElement>) => {
  const [size, setSize] = React.useState({ w: 800, h: 600 });
  React.useLayoutEffect(() => {
    const el = ref.current;
    if (!el) return;
    const obs = new ResizeObserver(() => {
      const w = el.clientWidth,
        h = el.clientHeight;
      setSize((prev) => (prev.w === w && prev.h === h ? prev : { w, h }));
    });
    obs.observe(el);
    setSize({ w: el.clientWidth, h: el.clientHeight });
    return () => obs.disconnect();
  }, [ref]);
  return size;
};

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
  const renderMeasurementOverlay = () => {
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
        <Line
          points={[x, hLineY, x + w, hLineY]}
          stroke="#111"
          listening={false}
        />
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
        <Line
          points={[vLineX, y, vLineX, y + h]}
          stroke="#111"
          listening={false}
        />
        <Line
          points={[vLineX - tick / 2, y, vLineX + tick / 2, y]}
          stroke="#111"
          listening={false}
        />
        <Line
          points={[
            vLineX - tick / 2,
            y + h,
            vLineX + tick / 2,
            y + h,
          ]}
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
        <Layer listening={false}>{renderMeasurementOverlay()}</Layer>
      </Stage>
    </div>
  );
};
