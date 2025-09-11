
'use client';
import React from 'react';
import { usePlanStore } from '../store/planStore';
import type { Issue } from '@planner/shared';

export const IssuePanel: React.FC = () => {
  const { issues, setSelected } = usePlanStore();
  if (!issues.length) return <div style={{ fontSize: 12, opacity: .7 }}>Проблем нет</div>;
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 8, maxHeight: 300, overflow: 'auto' }}>
      {issues.map((i, idx) => (
        <div key={idx} style={{ border: '1px solid #ddd', borderRadius: 8, padding: 8 }}>
          <div style={{ fontWeight: 600, fontSize: 13 }}>{i.code}</div>
          <div style={{ fontSize: 12 }}>{i.message}</div>
          {i.objectId && <button style={{ marginTop: 6 }} onClick={() => setSelected(i.objectId)}>Показать на плане</button>}
        </div>
      ))}
    </div>
  );
};
