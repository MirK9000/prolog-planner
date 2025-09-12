'use client';
import React from 'react';

export const useContainerSize = (ref: React.RefObject<HTMLDivElement>) => {
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
