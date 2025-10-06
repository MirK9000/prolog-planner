import { useCallback, useMemo, useState } from 'react';
import { clamp } from '@planner/geometry';

export interface CanvasView {
  zoom: number;
  panX: number;
  panY: number;
}

export interface UseCanvasViewOptions {
  roomSize: { W: number; H: number };
  containerSize: { w: number; h: number };
  pxPerMm: number;
  padding: number;
}

export interface ZoomAtCenterOptions {
  width?: number;
  height?: number;
}

const MIN_ZOOM = 0.2;
const MAX_ZOOM = 10;

const clampZoom = (value: number) => clamp(value, MIN_ZOOM, MAX_ZOOM);

export function useCanvasView({
  roomSize,
  containerSize,
  pxPerMm,
  padding,
}: UseCanvasViewOptions) {
  const [view, setView] = useState<CanvasView>({ zoom: 1, panX: 0, panY: 0 });

  const fit = useMemo(() => {
    const roomPxW = roomSize.W * pxPerMm;
    const roomPxH = roomSize.H * pxPerMm;
    const paddedW = roomPxW + padding * 2;
    const paddedH = roomPxH + padding * 2;

    if (!paddedW || !paddedH || !containerSize.w || !containerSize.h) {
      return 1;
    }

    const scale = Math.min(containerSize.w / paddedW, containerSize.h / paddedH) || 1;
    return Math.max(1e-6, Math.floor(scale * 1000) / 1000);
  }, [containerSize.h, containerSize.w, padding, pxPerMm, roomSize.H, roomSize.W]);

  const mm2px = pxPerMm * fit * view.zoom;

  const zoomAtPoint = useCallback(
    (sx: number, sy: number, factor: number) => {
      setView((prev) => {
        const nextZoom = clampZoom(prev.zoom * factor);
        if (nextZoom === prev.zoom) return prev;

        const baseX = padding;
        const baseY = padding;

        const oldMm2px = pxPerMm * fit * prev.zoom;
        const newMm2px = pxPerMm * fit * nextZoom;

        const newPanX =
          sx -
          baseX -
          ((sx - baseX - prev.panX) / oldMm2px) * newMm2px;
        const newPanY =
          sy -
          baseY -
          ((sy - baseY - prev.panY) / oldMm2px) * newMm2px;

        return {
          zoom: nextZoom,
          panX: newPanX,
          panY: newPanY,
        };
      });
    },
    [fit, padding, pxPerMm]
  );

  const zoomAtCenter = useCallback(
    (factor: number, size?: ZoomAtCenterOptions) => {
      const width = size?.width ?? containerSize.w;
      const height = size?.height ?? containerSize.h;
      const centerX = width / 2;
      const centerY = height / 2;
      zoomAtPoint(centerX, centerY, factor);
    },
    [containerSize.h, containerSize.w, zoomAtPoint]
  );

  const resetView = useCallback(() => {
    setView({ zoom: 1, panX: 0, panY: 0 });
  }, []);

  const setPan = useCallback((panX: number, panY: number) => {
    setView((prev) => {
      if (prev.panX === panX && prev.panY === panY) {
        return prev;
      }
      return { ...prev, panX, panY };
    });
  }, []);

  return {
    view,
    setView,
    fit,
    mm2px,
    zoomAtPoint,
    zoomAtCenter,
    resetView,
    setPan,
  };
}
