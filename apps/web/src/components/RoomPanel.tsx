'use client';
import React from 'react';
import { usePlanStore } from '../store/planStore';
import { snap } from '@planner/geometry';

const GRID_MM = 100; // шаг сетки для «прилипания» значений

export const RoomPanel: React.FC = () => {
  const { plan, updateRoom } = usePlanStore();
  const [W, setW] = React.useState<number>(plan.room.W);
  const [H, setH] = React.useState<number>(plan.room.H);

  // держим локальные поля синхронными при внешних изменениях
  React.useEffect(() => { setW(plan.room.W); setH(plan.room.H); }, [plan.room.W, plan.room.H]);

  const applyW = (value: number) => {
    const v = Math.max(1000, Math.min(200000, snap(Math.round(value), GRID_MM)));
    setW(v);
    updateRoom({ W: v });
  };
  const applyH = (value: number) => {
    const v = Math.max(1000, Math.min(200000, snap(Math.round(value), GRID_MM)));
    setH(v);
    updateRoom({ H: v });
  };

  const areaM2 = (W * H) / 1_000_000;
  const ratio = (W / H);

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 12, height: '100%' }}>
      <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 12 }}>
        <label style={{ display: 'flex', flexDirection: 'column', gap: 6 }}>
          <span style={{ fontSize: 12, color: '#6b7280' }}>Ширина (мм)</span>
          <input
            type="number"
            value={W}
            step={GRID_MM}
            min={1000}
            max={200000}
            onChange={(e)=> setW(Number(e.target.value))}
            onBlur={()=> applyW(W)}
            onKeyDown={(e)=> { if (e.key === 'Enter') applyW(W); }}
            style={{ padding: '6px 8px', border: '1px solid #ddd', borderRadius: 6 }}
          />
        </label>
        <label style={{ display: 'flex', flexDirection: 'column', gap: 6 }}>
          <span style={{ fontSize: 12, color: '#6b7280' }}>Высота (мм)</span>
          <input
            type="number"
            value={H}
            step={GRID_MM}
            min={1000}
            max={200000}
            onChange={(e)=> setH(Number(e.target.value))}
            onBlur={()=> applyH(H)}
            onKeyDown={(e)=> { if (e.key === 'Enter') applyH(H); }}
            style={{ padding: '6px 8px', border: '1px solid #ddd', borderRadius: 6 }}
          />
        </label>
      </div>

      <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 12, fontSize: 12 }}>
        <div>
          <div style={{ color: '#6b7280' }}>Площадь</div>
          <div><b>{areaM2.toFixed(2)}</b> м²</div>
        </div>
        <div>
          <div style={{ color: '#6b7280' }}>Соотношение сторон</div>
          <div><b>{ratio.toFixed(3)}</b></div>
        </div>
      </div>

      <div style={{ fontSize: 12, color: '#6b7280' }}>
        Значения «прилипают» к шагу <b>{GRID_MM} мм</b>. Для быстрого обзора нажми клавишу <b>F</b> — «подогнать к экрану».
      </div>
    </div>
  );
};
