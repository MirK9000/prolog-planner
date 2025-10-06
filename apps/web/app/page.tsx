'use client';
import React from 'react';
import dynamic from 'next/dynamic';
import { IssuePanel } from '../src/components/IssuePanel';
import { ObjectList } from '../src/components/ObjectList';
import { RoomPanel } from '../src/components/RoomPanel';     // ⬅️ новый импорт
import { ObjectPalette } from '../src/components/ObjectPalette';
import { usePlanStore } from '../src/store/planStore';
import { runAllBasicRules } from '@planner/rules';
import { toProlog } from '@planner/serializer';
import type { SolutionDto } from '../src/types/solver';

const EditorCanvas = dynamic(
  () => import('../src/components/EditorCanvas').then(m => m.EditorCanvas),
  { ssr: false }
);

const API_BASE_URL = process.env.NEXT_PUBLIC_API_URL ?? 'http://localhost:3333';

export default function Home() {
  const {
    plan,
    setPlan,
    setIssues,
    issues,
    solving,
    solverError,
    setSolving,
    setSolverError,
  } = usePlanStore();
  const [tab, setTab] = React.useState<'room' | 'objects' | 'issues'>('objects'); // ⬅️ добавили 'room'

  const checkPlan = () => setIssues(runAllBasicRules(plan));

  const exportPlan = async () => {
    try {
      const res = await fetch('http://localhost:3333/export/plan.pl', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(plan)
      });
      const text = await res.text();
      const blob = new Blob([text], { type: 'text/plain;charset=utf-8' });
      const a = document.createElement('a');
      a.href = URL.createObjectURL(blob); a.download = 'plan.pl'; a.click(); URL.revokeObjectURL(a.href);
    } catch {
      const text = toProlog(plan);
      const blob = new Blob([text], { type: 'text/plain;charset=utf-8' });
      const a = document.createElement('a');
      a.href = URL.createObjectURL(blob); a.download = 'plan.pl'; a.click(); URL.revokeObjectURL(a.href);
    }
  };

  const importSolution = () => {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = 'application/json';
    input.onchange = async () => {
      const file = input.files?.[0];
      if (!file) return;
      try {
        const text = await file.text();
        const data = JSON.parse(text);
        const desks = Array.isArray(data.desks) ? data.desks : [];
        const objects = plan.objects.filter(o => o.type !== 'workplace');
        for (const d of desks) {
          const orientation =
            typeof d.orientation === 'number'
              ? (((d.orientation % 4) + 4) % 4) as 0 | 1 | 2 | 3
              : undefined;
          objects.push({
            id: d.id ?? usePlanStore.getState().nextIdForType('workplace'),
            type: 'workplace',
            rect: { X: d.rect.x, Y: d.rect.y, W: d.rect.w, H: d.rect.h },
            properties: [],
            orientation,
          });
        }
        let task = plan.task;
        if (desks.length > 0 && desks[0].base_size) {
          task = { count: desks.length, size: { W: desks[0].base_size.w, H: desks[0].base_size.h } };
        }
        setPlan({ ...plan, objects, task });
        setTab('objects');
      } catch (e) {
        console.error(e);
        alert('Не удалось прочитать файл решения');
      }
    };
    input.click();
  };

  const solvePlan = async () => {
    if (!plan.task) {
      alert('Сначала укажите количество и размер рабочих мест в разделе «Помещение».');
      return;
    }

    if (plan.objects.some(o => o.type === 'workplace')) {
      const replace = window.confirm('Заменить существующие рабочие места на решение решателя?');
      if (!replace) {
        return;
      }
    }

    setSolving(true);
    setSolverError(null);

    try {
      const response = await fetch(`${API_BASE_URL.replace(/\/$/, '')}/solver/solve`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(plan),
      });

      let result: SolutionDto | null = null;
      try {
        result = await response.json();
      } catch (error) {
        console.error('Failed to parse solver response', error);
      }

      if (!result) {
        setSolverError('Не удалось прочитать ответ решателя. Попробуйте еще раз.');
        return;
      }

      if (!response.ok && !result.success) {
        setSolverError(result.error?.message ?? 'Решатель вернул ошибку.');
        return;
      }

      if (!result.success) {
        const details = result.error?.details && typeof result.error.details === 'object'
          ? ` (подробнее: ${JSON.stringify(result.error.details)})`
          : '';
        setSolverError(`${result.error?.message ?? 'Решатель вернул ошибку.'}${details}`);
        return;
      }

      const desks = result.solution?.desks ?? [];
      const preserved = plan.objects.filter(o => o.type !== 'workplace');
      const workspace = usePlanStore.getState();
      const nextObjects = desks.map((desk) => {
        const normalizedOrientation = typeof desk.orientation === 'number'
          ? (((Math.round(desk.orientation) % 4) + 4) % 4) as 0 | 1 | 2 | 3
          : undefined;
        const identifier = desk.id && desk.id.length > 0
          ? desk.id
          : workspace.nextIdForType('workplace');
        return {
          id: identifier,
          type: 'workplace' as const,
          rect: {
            X: desk.rect.x,
            Y: desk.rect.y,
            W: desk.rect.w,
            H: desk.rect.h,
          },
          properties: [],
          orientation: normalizedOrientation,
        };
      });

      const updatedPlan = {
        ...plan,
        objects: [...preserved, ...nextObjects],
        task: plan.task
          ? { ...plan.task, count: result.metadata?.placedDesks ?? plan.task.count }
          : plan.task,
      };

      setPlan(updatedPlan);
      setTab('objects');

      const placed = result.metadata?.placedDesks ?? desks.length;
      const timeMs = result.metadata?.executionTime ?? 0;
      const seconds = (timeMs / 1000).toFixed(1);
      alert(`Успешно размещено ${placed} рабочих мест за ${seconds} сек.`);
    } catch (error) {
      console.error('Solver request failed', error);
      setSolverError('Не удалось подключиться к решателю. Проверьте, что сервер запущен.');
    } finally {
      setSolving(false);
    }
  };

  return (
    <main style={{
      display: 'grid', gridTemplateColumns: '1fr 380px', gap: 16,
      width: '100vw', height: '100vh', padding: 16, boxSizing: 'border-box', overflow: 'hidden'
    }}>
      <section style={{ display: 'flex', flexDirection: 'column', gap: 8, minWidth: 0, minHeight: 0 }}>
        <div style={{ display: 'flex', flexDirection: 'column', gap: 8, flex: '0 0 auto' }}>
          <div style={{ display: 'flex', gap: 8, flexWrap: 'wrap', alignItems: 'center' }}>
            <button onClick={checkPlan}>Проверить план</button>
            <button onClick={exportPlan}>Экспорт в Prolog</button>
            <button onClick={importSolution}>Импорт решения</button>
            <button
              onClick={solvePlan}
              disabled={solving || !plan.task}
              className="solve-button"
              title="Автоматически разместить рабочие места"
            >
              {solving ? (
                <>
                  <span className="spinner" aria-hidden="true" />
                  <span>Решение в процессе...</span>
                </>
              ) : (
                <>
                  <span aria-hidden="true">🪄</span>
                  <span>Решить планировку</span>
                </>
              )}
            </button>
          </div>
          {!plan.task && (
            <div style={{ fontSize: 12, color: '#6b7280' }}>
              Укажите количество и размер рабочих мест в разделе «Помещение», чтобы запустить решатель.
            </div>
          )}
          {solverError && (
            <div className="solver-error-panel" role="alert">
              <span aria-hidden="true">⚠️</span>
              <div style={{ flex: 1 }}>
                <strong>Ошибка решения:</strong>
                <p style={{ margin: '4px 0 0 0' }}>{solverError}</p>
              </div>
              <button onClick={() => setSolverError(null)} aria-label="Закрыть уведомление">
                ×
              </button>
            </div>
          )}
          <ObjectPalette /> {/* ⬅️ палитра типов */}
        </div>
        <div style={{ flex: 1, minHeight: 0 }}>
          <EditorCanvas />
        </div>
      </section>

      <aside style={{ minWidth: 0, minHeight: 0, height: '100%', overflow: 'hidden', display: 'flex', flexDirection: 'column' }}>
        {/* Tabs */}
        <div style={{ display: 'flex', gap: 8, marginBottom: 8 }}>
          <button
            onClick={()=> setTab('room')}
            style={{
              padding: '6px 10px',
              borderRadius: 8,
              border: '1px solid #ddd',
              background: tab === 'room' ? '#eef2ff' : '#fff'
            }}
          >
            Помещение
          </button>
          <button
            onClick={()=> setTab('objects')}
            style={{
              padding: '6px 10px',
              borderRadius: 8,
              border: '1px solid #ddd',
              background: tab === 'objects' ? '#eef2ff' : '#fff'
            }}
          >
            Объекты
          </button>
          <button
            onClick={()=> setTab('issues')}
            style={{
              padding: '6px 10px',
              borderRadius: 8,
              border: '1px solid #ddd',
              background: tab === 'issues' ? '#eef2ff' : '#fff'
            }}
          >
            Проблемы ({issues.length})
          </button>
        </div>

        {/* Tab content */}
        <div style={{ flex: 1, minHeight: 0, overflow: 'auto' }}>
          {tab === 'room' ? (
            <RoomPanel />
          ) : tab === 'objects' ? (
            <ObjectList />
          ) : (
            <IssuePanel />
          )}
        </div>
      </aside>
    </main>
  );
}
