'use client';
import React from 'react';
import { usePlanStore } from '../store/planStore';
import { snap } from '@planner/geometry';

const GRID_MM = 100; // шаг сетки для «прилипания» значений
const WORKPLACE_SIZE = { W: 1400, H: 800 };
const DEFAULT_TASK_COUNT = 12;

export const RoomPanel: React.FC = () => {
  const { plan, updateRoom, updateTask, clearWorkplaces } = usePlanStore();
  const [W, setW] = React.useState<number>(plan.room.W);
  const [H, setH] = React.useState<number>(plan.room.H);
  const [taskCount, setTaskCount] = React.useState<number>(plan.task?.count ?? DEFAULT_TASK_COUNT);

  // держим локальные поля синхронными при внешних изменениях
  React.useEffect(() => {
    setW(plan.room.W);
    setH(plan.room.H);
  }, [plan.room.W, plan.room.H]);

  React.useEffect(() => {
    setTaskCount(plan.task?.count ?? DEFAULT_TASK_COUNT);
  }, [plan.task?.count]);

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

  const applyTaskCount = (count: number) => {
    const validated = Math.max(1, Math.min(100, Math.round(count)));
    setTaskCount(validated);
    updateTask({ count: validated, size: WORKPLACE_SIZE });
  };

  const currentWorkplaces = plan.objects.filter(o => o.type === 'workplace').length;

  const clearSolution = () => {
    if (currentWorkplaces === 0) return;

    const confirmed = window.confirm(
      `Удалить ${currentWorkplaces} рабочих мест с плана?\n\nЭто действие нельзя отменить.`
    );

    if (!confirmed) return;

    clearWorkplaces();
  };

  const areaM2 = (W * H) / 1_000_000;
  const ratio = H === 0 ? 0 : W / H;
  const taskSize = plan.task?.size ?? WORKPLACE_SIZE;

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
            onChange={(e) => setW(Number(e.target.value))}
            onBlur={() => applyW(W)}
            onKeyDown={(e) => { if (e.key === 'Enter') applyW(W); }}
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
            onChange={(e) => setH(Number(e.target.value))}
            onBlur={() => applyH(H)}
            onKeyDown={(e) => { if (e.key === 'Enter') applyH(H); }}
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

      <div style={{
        marginTop: 24,
        paddingTop: 16,
        borderTop: '1px solid #e5e7eb'
      }}>
        <h3 style={{
          fontSize: 14,
          fontWeight: 600,
          marginBottom: 12,
          color: '#111827'
        }}>
          Задача размещения
        </h3>

        <label style={{ display: 'flex', flexDirection: 'column', gap: 6 }}>
          <span style={{ fontSize: 12, color: '#6b7280' }}>
            Количество рабочих мест
          </span>
          <input
            type="number"
            min={1}
            max={100}
            step={1}
            value={taskCount}
            onChange={(e) => setTaskCount(Number(e.target.value))}
            onBlur={() => applyTaskCount(taskCount)}
            onKeyDown={(e) => { if (e.key === 'Enter') applyTaskCount(taskCount); }}
            style={{
              padding: '6px 8px',
              border: '1px solid #ddd',
              borderRadius: 6
            }}
            placeholder="Например: 12"
          />
        </label>

        <div style={{
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'center',
          padding: '8px 12px',
          background: '#f9fafb',
          borderRadius: 6,
          fontSize: 13,
          marginTop: 12
        }}>
          <span style={{ color: '#6b7280' }}>Размер одного РМ:</span>
          <strong>{taskSize.W} × {taskSize.H} мм</strong>
        </div>

        <div style={{
          marginTop: 12,
          padding: 12,
          background: '#f9fafb',
          borderRadius: 8,
          fontSize: 13
        }}>
          <div style={{ display: 'flex', justifyContent: 'space-between', marginBottom: 6 }}>
            <span style={{ color: '#6b7280' }}>Размещено на плане:</span>
            <strong>{currentWorkplaces} РМ</strong>
          </div>

          {plan.task && (
            <div style={{ display: 'flex', justifyContent: 'space-between' }}>
              <span style={{ color: '#6b7280' }}>Задано для решателя:</span>
              <strong>{plan.task.count} РМ</strong>
            </div>
          )}

          {plan.task && currentWorkplaces !== plan.task.count && (
            <div style={{
              marginTop: 8,
              padding: 8,
              background: '#fef3c7',
              borderRadius: 6,
              fontSize: 12,
              color: '#92400e'
            }}>
              ⚠️ Количество на плане не совпадает с заданным
            </div>
          )}
        </div>

        <button
          onClick={clearSolution}
          disabled={currentWorkplaces === 0}
          style={{
            marginTop: 12,
            width: '100%',
            padding: '8px 12px',
            fontSize: 13,
            fontWeight: 500,
            borderRadius: 8,
            border: 'none',
            background: currentWorkplaces > 0 ? '#ef4444' : '#e5e7eb',
            color: currentWorkplaces > 0 ? '#fff' : '#9ca3af',
            cursor: currentWorkplaces > 0 ? 'pointer' : 'not-allowed',
            transition: 'all 0.2s'
          }}
          title={currentWorkplaces > 0 ? 'Удалить все рабочие места с плана' : 'Нет рабочих мест для удаления'}
        >
          🗑️ Очистить решение ({currentWorkplaces} РМ)
        </button>
      </div>

      <div style={{ fontSize: 12, color: '#6b7280', marginTop: 'auto' }}>
        Значения «прилипают» к шагу <b>{GRID_MM} мм</b>. Для быстрого обзора нажми клавишу <b>F</b> — «подогнать к экрану».
      </div>
    </div>
  );
};
