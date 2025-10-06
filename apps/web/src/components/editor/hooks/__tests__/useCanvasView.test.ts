import { act, renderHook } from '@testing-library/react';
import { useCanvasView } from '../useCanvasView';

describe('useCanvasView', () => {
  const defaultOptions = {
    roomSize: { W: 10000, H: 8000 },
    containerSize: { w: 800, h: 600 },
    pxPerMm: 0.1,
    padding: 20,
  };

  it('calculates fit within expected range', () => {
    const { result } = renderHook(() => useCanvasView(defaultOptions));

    expect(result.current.fit).toBeGreaterThan(0);
    expect(result.current.fit).toBeLessThanOrEqual(1);
  });

  it('updates zoom at center', () => {
    const { result } = renderHook(() => useCanvasView(defaultOptions));

    act(() => {
      result.current.zoomAtCenter(2);
    });

    expect(result.current.view.zoom).toBeCloseTo(2, 5);
  });

  it('resets view to defaults', () => {
    const { result } = renderHook(() => useCanvasView(defaultOptions));

    act(() => {
      result.current.zoomAtCenter(2);
      result.current.setPan(100, 50);
    });

    act(() => {
      result.current.resetView();
    });

    expect(result.current.view).toEqual({ zoom: 1, panX: 0, panY: 0 });
  });

  it('clamps zoom to min and max', () => {
    const { result } = renderHook(() => useCanvasView(defaultOptions));

    act(() => {
      result.current.zoomAtCenter(100);
    });

    expect(result.current.view.zoom).toBeLessThanOrEqual(10);

    act(() => {
      result.current.zoomAtCenter(0.001);
    });

    expect(result.current.view.zoom).toBeGreaterThanOrEqual(0.2);
  });

  it('adjusts pan when zooming at point', () => {
    const { result } = renderHook(() => useCanvasView(defaultOptions));
    const initialPan = result.current.view.panX;

    act(() => {
      result.current.zoomAtPoint(400, 300, 1.5);
    });

    expect(result.current.view.panX).not.toBe(initialPan);
  });
});
