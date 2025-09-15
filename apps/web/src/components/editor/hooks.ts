'use client';
import React from 'react';
import { usePlanStore } from '../../store/planStore';

export type View = { zoom: number; panX: number; panY: number };

export const useContainerSize = (ref: React.RefObject<HTMLDivElement | null>) => {
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

export const useEditorHotkeys = (
  zoomAtCenter: (factor: number) => void,
  setView: React.Dispatch<React.SetStateAction<View>>,
  setGhost: (g: any) => void,
  updateGhostAt: () => void
) => {
  const { setPlacingType, deleteSelected, placingType, copySelected, copied } = usePlanStore();
  const spacePressed = React.useRef(false);

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
      if (e.key === '0' || e.key.toLowerCase() === 'f') {
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
      if ((e.key.toLowerCase() === 'c') && (e.metaKey || e.ctrlKey) && !placingType) {
        e.preventDefault();
        copySelected();
      }
      if ((e.key.toLowerCase() === 'v') && (e.metaKey || e.ctrlKey)) {
        if (copied) {
          e.preventDefault();
          setPlacingType(copied.type);
          setGhost({ ...copied.rect });
          updateGhostAt();
        }
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
  }, [zoomAtCenter, setView, setPlacingType, deleteSelected, placingType, setGhost, updateGhostAt, copySelected, copied]);

  return spacePressed;
};
