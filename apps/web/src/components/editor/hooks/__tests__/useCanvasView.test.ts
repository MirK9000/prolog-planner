import { act, renderHook } from '@testing-library/react';
import { useCanvasView } from '../useCanvasView';

describe('useCanvasView', () => {
  const defaultOptions = {
    roomSize: { W: 10000, H: 8000 },
    containerSize: { w: 800, h: 600 },
    pxPerMm: 0.1,
    padding: 20,
  } as const;

  it('calculates fit within expected bounds', () => {
    const { result } = renderHook(() => useCanvasView(defaultOptions));

    expect(result.current.fit).toBeGreaterThan(0);
    expect(result.current.fit).toBeLessThanOrEqual(1);
  });

  it('zooms at center and respects max zoom', () => {
    const { result } = renderHook(() => useCanvasView(defaultOptions));

    act(() => {
      result.current.zoomAtCenter(2);
      result.current.zoomAtCenter(2);
      result.current.zoomAtCenter(2);
    });

    expect(result.current.view.zoom).toBeLessThanOrEqual(10);
    expect(result.current.view.zoom).toBeGreaterThan(1);
  });

  it('resets the view state', () => {
    const { result } = renderHook(() => useCanvasView(defaultOptions));

    act(() => {
      result.current.zoomAtCenter(1.5);
      result.current.setPan(100, 150);
    });

    expect(result.current.view.zoom).not.toBe(1);
    expect(result.current.view.panX).toBe(100);

    act(() => {
      result.current.resetView();
    });

    expect(result.current.view).toEqual({ zoom: 1, panX: 0, panY: 0 });
  });

  it('handles wheel events using pointer position from stageRef', () => {
    let prevented = false;
    const preventDefault = () => {
      prevented = true;
    };
    const stageRef = {
      current: {
        getPointerPosition: () => ({ x: 100, y: 120 }),
        width: () => 800,
        height: () => 600,
      },
    } as const;

    const { result } = renderHook(() =>
      useCanvasView({ ...defaultOptions, stageRef })
    );

    act(() => {
      result.current.handleWheel({
        evt: { preventDefault, deltaY: -100 },
      } as any);
    });

    expect(prevented).toBe(true);
    expect(result.current.view.zoom).toBeGreaterThan(1);
  });
});
