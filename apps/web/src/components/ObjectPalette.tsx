'use client';
import React from 'react';
import { usePlanStore } from '../store/planStore';

// те же русские подписи и цвета, что на канве
export const TYPE_LABEL: Record<string, string> = {
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

export const TYPE_COLOR: Record<string, { fill: string; stroke: string }> = {
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

const TYPES_ORDER = [
  'workplace','door','window','column',
  'fire_extinguisher','fire_alarm','electrical_shield',
  'cabinet','net_cabinet','comms_block'
];

export const ObjectPalette: React.FC = () => {
  const { placingType, setPlacingType } = usePlanStore();

  return (
    <div style={{ display: 'flex', alignItems: 'center', gap: 8, overflow: 'hidden' }}>
      <div style={{ whiteSpace: 'nowrap', overflowX: 'auto', paddingBottom: 4 }}>
        {TYPES_ORDER.map(type => {
          const active = placingType === type;
          const color = TYPE_COLOR[type]?.stroke ?? '#6b7280';
          return (
            <button
              key={type}
              onClick={() => setPlacingType(active ? undefined : type)}
              title={`Разместить: ${TYPE_LABEL[type] ?? type}`}
              style={{
                display: 'inline-flex', alignItems: 'center', gap: 8,
                padding: '6px 10px', marginRight: 8,
                borderRadius: 10, border: `1px solid ${active ? '#6366f1' : '#e5e7eb'}`,
                background: active ? '#eef2ff' : '#fff', cursor: 'pointer'
              }}
            >
              <span style={{ width: 12, height: 12, borderRadius: 9999, background: color }} />
              <span style={{ fontSize: 13 }}>{TYPE_LABEL[type] ?? type}</span>
            </button>
          );
        })}
      </div>
      {placingType && (
        <div style={{ marginLeft: 8, fontSize: 12, color: '#6b7280' }}>
          Режим размещения: <b>{TYPE_LABEL[placingType] ?? placingType}</b> — кликните на план. <span style={{ opacity: .8 }}>Esc — отмена</span>
        </div>
      )}
    </div>
  );
};
