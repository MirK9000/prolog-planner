import * as React from 'react';
import type { KonvaEventObject } from 'konva/lib/Node';
import { clamp } from '@planner/geometry';

export type CanvasView = {
  zoom: number;
  panX: number;
  panY: number;
};

export interface UseCanvasViewOptions {
  roomSize: { W: number; H: number };
  containerSize: { w: number; h: number };
  pxPerMm: number;
  padding: number;
  stageRef?: React.RefObject<any>;
  minZoom?: number;
  maxZoom?: number;
}

export interface UseCanvasViewResult {
  view: CanvasView;
  setView: React.Dispatch<React.SetStateAction<CanvasView>>;
  fit: number;
  mm2px: number;
  zoomAt: (x: number, y: number, factor: number) => void;
  zoomAtCenter: (factor: number) => void;
  resetView: () => void;
  setPan: (panX: number, panY: number) => void;
  applyPanDelta: (dx: number, dy: number) => void;
  handleWheel: (evt: KonvaEventObject<WheelEvent>) => void;
}

/**
 * Manages zooming and panning state for the editor canvas.
 */
export function useCanvasView({
  roomSize,
  containerSize,
  pxPerMm,
  padding,
  stageRef,
  minZoom = 0.2,
  maxZoom = 10,
}: UseCanvasViewOptions): UseCanvasViewResult {
  const [view, setView] = React.useState<CanvasView>({ zoom: 1, panX: 0, panY: 0 });

  const fit = React.useMemo(() => {
    const roomPxW = roomSize.W * pxPerMm;
    const roomPxH = roomSize.H * pxPerMm;
    const scale =
      Math.min(
        containerSize.w / (roomPxW + padding * 2),
        containerSize.h / (roomPxH + padding * 2)
      ) || 1;
    return Math.max(1e-6, Math.floor(scale * 1000) / 1000);
  }, [containerSize.h, containerSize.w, padding, pxPerMm, roomSize.H, roomSize.W]);

  const mm2px = pxPerMm * fit * view.zoom;

  const zoomAt = React.useCallback(
    (sx: number, sy: number, factor: number) => {
      setView((prev) => {
        const oldZoom = prev.zoom;
        const nextZoom = clamp(oldZoom * factor, minZoom, maxZoom);
        if (nextZoom === oldZoom) {
          return prev;
        }

        const baseX = padding;
        const baseY = padding;
        const oldMm2px = pxPerMm * fit * oldZoom;
        const newMm2px = pxPerMm * fit * nextZoom;

        const newPanX = sx - baseX - ((sx - baseX - prev.panX) / oldMm2px) * newMm2px;
        const newPanY = sy - baseY - ((sy - baseY - prev.panY) / oldMm2px) * newMm2px;

        return {
          zoom: nextZoom,
          panX: newPanX,
          panY: newPanY,
        };
      });
    },
    [fit, maxZoom, minZoom, padding, pxPerMm]
  );

  const zoomAtCenter = React.useCallback(
    (factor: number) => {
      const stage = stageRef?.current as { width?: () => number; height?: () => number } | null;
      const sx = (stage?.width?.() ?? containerSize.w) / 2;
      const sy = (stage?.height?.() ?? containerSize.h) / 2;
      zoomAt(sx, sy, factor);
    },
    [containerSize.h, containerSize.w, stageRef, zoomAt]
  );

  const resetView = React.useCallback(() => {
    setView({ zoom: 1, panX: 0, panY: 0 });
  }, []);

  const setPan = React.useCallback((panX: number, panY: number) => {
    setView((prev) => ({ ...prev, panX, panY }));
  }, []);

  const applyPanDelta = React.useCallback((dx: number, dy: number) => {
    setView((prev) => ({ ...prev, panX: prev.panX + dx, panY: prev.panY + dy }));
  }, []);

  const handleWheel = React.useCallback(
    (evt: KonvaEventObject<WheelEvent>) => {
      evt.evt.preventDefault();
      const stage = stageRef?.current;
      const pointer = stage?.getPointerPosition?.();
      if (!pointer) return;
      const direction = evt.evt.deltaY > 0 ? 1 / 1.1 : 1.1;
      zoomAt(pointer.x, pointer.y, direction);
    },
    [stageRef, zoomAt]
  );

  return {
    view,
    setView,
    fit,
    mm2px,
    zoomAt,
    zoomAtCenter,
    resetView,
    setPan,
    applyPanDelta,
    handleWheel,
  };
}
