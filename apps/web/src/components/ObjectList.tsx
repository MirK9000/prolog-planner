'use client';
import React from 'react';
import { usePlanStore } from '../store/planStore';
import { isOnWall } from '@planner/geometry';

// подписи типов (RU)
const TYPE_LABEL: Record<string, string> = {
  door: 'Дверь',
  window: 'Окно',
  column: 'Колонна',
  workplace: 'Место',
  fire_extinguisher: 'Огнетуш.',
  fire_alarm: 'Пож. изв.',
  electrical_shield: 'Эл. щит',
  comms_block: 'Связь',
  net_cabinet: 'Сет. шкаф',
  cabinet: 'Шкаф',
  wall: 'Стена',
};

// цвета по типам (в унисон с Canvas)
const TYPE_COLOR: Record<string, { fill: string; stroke: string }> = {
  door: { fill: '#f59e0b33', stroke: '#f59e0b' },
  window: { fill: '#38bdf833', stroke: '#38bdf8' },
  column: { fill: '#94a3b833', stroke: '#94a3b8' },
  workplace: { fill: '#22c55e33', stroke: '#22c55e' },
  fire_extinguisher: { fill: '#ef444433', stroke: '#ef4444' },
  fire_alarm: { fill: '#f9731633', stroke: '#f97316' },
  electrical_shield: { fill: '#06b6d433', stroke: '#06b6d4' },
  comms_block: { fill: '#e879f933', stroke: '#e879f9' },
  net_cabinet: { fill: '#10b98133', stroke: '#10b981' },
  cabinet: { fill: '#d946ef33', stroke: '#d946ef' },
  wall: { fill: '#9ca3af33', stroke: '#9ca3af' },
};

const fmtM2 = (mm2: number) => (mm2 / 1_000_000).toFixed(2); // м² из мм²

const PropInput: React.FC<{ id: string; kind: 'radius' | 'capacity'; initial: number; width: number }> = ({ id, kind, initial, width }) => {
  const { setProperty } = usePlanStore();
  const [val, setVal] = React.useState(initial.toString());

  React.useEffect(() => { setVal(initial.toString()); }, [initial]);

  const commit = () => {
    const n = Number(val);
    setProperty(id, { kind, value: isNaN(n) ? 0 : n });
  };

  return (
    <input
      type="text"
      inputMode="numeric"
      value={val}
      onChange={e => setVal(e.target.value)}
      onClick={e => e.stopPropagation()}
      onPointerDown={e => e.stopPropagation()}
      onBlur={commit}
      onKeyDown={e => { if (e.key === 'Enter') commit(); }}
      style={{ width, padding: '2px 4px', border: '1px solid #ddd', borderRadius: 4 }}
    />
  );
};

export const ObjectList: React.FC = () => {
  const { plan, selectedId, setSelected, deleteObject } = usePlanStore();
  const [q, setQ] = React.useState('');

  const rows = React.useMemo(() => {
    const arr = [...plan.objects];
    // сортируем: тип → id
    arr.sort((a, b) => (a.type.localeCompare(b.type) || a.id.localeCompare(b.id)));
    return arr.filter(o =>
      o.id.toLowerCase().includes(q.toLowerCase()) ||
      (TYPE_LABEL[o.type] ?? o.type).toLowerCase().includes(q.toLowerCase())
    );
  }, [plan.objects, q]);

  const exportCSV = () => {
    const header = ['id','type','type_label','x_mm','y_mm','w_mm','h_mm','area_m2','requires_wall','on_wall'];
    const lines = rows.map(o => {
      const onWall = isOnWall(o.rect, plan.room);
      const area = (o.rect.W * o.rect.H);
      return [
        o.id,
        o.type,
        (TYPE_LABEL[o.type] ?? o.type),
        o.rect.X, o.rect.Y, o.rect.W, o.rect.H,
        fmtM2(area),
        o.requiresWallAnchor ? 'yes' : 'no',
        onWall ? 'yes' : 'no',
      ].join(',');
    });
    const csv = [header.join(','), ...lines].join('\n');
    const blob = new Blob([csv], { type: 'text/csv;charset=utf-8' });
    const a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = 'objects.csv';
    a.click();
    URL.revokeObjectURL(a.href);
  };

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 8, height: '100%' }}>
      <div style={{ display: 'flex', gap: 8 }}>
        <input
          value={q}
          onChange={(e)=> setQ(e.target.value)}
          placeholder="Поиск: id или тип…"
          style={{ flex: 1, padding: '6px 8px', border: '1px solid #ddd', borderRadius: 6 }}
        />
        <button onClick={exportCSV}>CSV</button>
      </div>

      <div style={{ fontSize: 12, color: '#6b7280' }}>
        Всего: <b>{plan.objects.length}</b>, отфильтровано: <b>{rows.length}</b>
      </div>

      <div style={{ overflow: 'auto', border: '1px solid #eee', borderRadius: 8 }}>
        <table style={{ width: '100%', borderCollapse: 'collapse', fontSize: 12 }}>
          <thead style={{ position: 'sticky', top: 0, background: '#fafafa' }}>
            <tr>
              <th style={th}>Тип</th>
              <th style={th}>ID</th>
              <th style={th}>X (мм)</th>
              <th style={th}>Y (мм)</th>
              <th style={th}>W (мм)</th>
              <th style={th}>H (мм)</th>
              <th style={th}>R (мм)</th>
              <th style={th}>Емк.</th>
              <th style={th}>Площадь (м²)</th>
              <th style={th}>Нужна привязка</th>
              <th style={th}>На стене</th>
              <th style={th}></th>
            </tr>
          </thead>
          <tbody>
            {rows.map(o => {
              const color = TYPE_COLOR[o.type]?.stroke ?? '#6b7280';
              const onWall = isOnWall(o.rect, plan.room);
              const sel = selectedId === o.id;
              const radius = o.properties.find(p => p.kind === 'radius')?.value ?? 0;
              const capacity = o.properties.find(p => p.kind === 'capacity')?.value ?? 0;
              return (
                <tr
                  key={o.id}
                  onClick={()=> setSelected(o.id)}
                  style={{
                    background: sel ? '#eef2ff' : undefined,
                    cursor: 'pointer'
                  }}
                >
                  <td style={td}>
                    <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
                      <span style={{
                        width: 10, height: 10, borderRadius: 9999,
                        background: color
                      }} />
                      {TYPE_LABEL[o.type] ?? o.type}
                    </div>
                  </td>
                  <td style={tdMono}>{o.id}</td>
                  <td style={tdNum}>{o.rect.X}</td>
                  <td style={tdNum}>{o.rect.Y}</td>
                  <td style={tdNum}>{o.rect.W}</td>
                  <td style={tdNum}>{o.rect.H}</td>
                  <td style={tdNum}>
                    {o.type === 'comms_block' && (
                      <PropInput id={o.id} kind="radius" initial={radius} width={60} />
                    )}
                  </td>
                  <td style={tdNum}>
                    {o.type === 'comms_block' && (
                      <PropInput id={o.id} kind="capacity" initial={capacity} width={40} />
                    )}
                  </td>
                  <td style={tdNum}>{fmtM2(o.rect.W * o.rect.H)}</td>
                  <td style={td}>{o.requiresWallAnchor ? 'Да' : 'Нет'}</td>
                  <td style={{ ...td, color: onWall ? '#16a34a' : '#ef4444' }}>{onWall ? 'Да' : 'Нет'}</td>
                  <td style={{ ...td, textAlign: 'right', whiteSpace: 'nowrap' }}>
                    <button
                        onClick={(ev)=> { ev.stopPropagation(); setSelected(o.id); }}
                        title="Показать на плане"
                        style={{ marginRight: 8 }}
                    >
                        Показать
                    </button>
                    <button
                        onClick={(ev)=> {
                        ev.stopPropagation();
                        if (confirm(`Удалить объект ${o.id}?`)) {
                            // импортируйте deleteObject сверху: const { plan, selectedId, setSelected, deleteObject } = usePlanStore();
                            deleteObject(o.id);
                        }
                        }}
                        title="Удалить"
                        style={{ color: '#b91c1c' }}
                    >
                        🗑
                    </button>
                    </td>
                </tr>
              );
            })}
            {!rows.length && (
              <tr><td colSpan={12} style={{ padding: 12, color: '#6b7280' }}>Ничего не найдено…</td></tr>
            )}
          </tbody>
        </table>
      </div>
    </div>
  );
};

const th: React.CSSProperties = { textAlign: 'left', padding: '8px 10px', borderBottom: '1px solid #eee', fontWeight: 600, position: 'sticky', top: 0 };
const td: React.CSSProperties = { padding: '8px 10px', borderBottom: '1px solid #f3f4f6' };
const tdMono: React.CSSProperties = { ...td, fontFamily: 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace' };
const tdNum: React.CSSProperties = { ...td, textAlign: 'right' };
