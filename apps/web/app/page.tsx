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

const EditorCanvas = dynamic(
  () => import('../src/components/EditorCanvas').then(m => m.EditorCanvas),
  { ssr: false }
);

export default function Home() {
  const { plan, setPlan, setIssues, issues } = usePlanStore();
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
          objects.push({
            id: d.id ?? usePlanStore.getState().nextIdForType('workplace'),
            type: 'workplace',
            rect: { X: d.rect.x, Y: d.rect.y, W: d.rect.w, H: d.rect.h },
            properties: [],
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

  return (
    <main style={{
      display: 'grid', gridTemplateColumns: '1fr 380px', gap: 16,
      width: '100vw', height: '100vh', padding: 16, boxSizing: 'border-box', overflow: 'hidden'
    }}>
      <section style={{ display: 'flex', flexDirection: 'column', gap: 8, minWidth: 0, minHeight: 0 }}>
        <div style={{ display: 'flex', flexDirection: 'column', gap: 8, flex: '0 0 auto' }}>
          <div style={{ display: 'flex', gap: 8 }}>
            <button onClick={checkPlan}>Проверить план</button>
            <button onClick={exportPlan}>Экспорт в Prolog</button>
            <button onClick={importSolution}>Импорт решения</button>
          </div>
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
