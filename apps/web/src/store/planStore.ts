'use client';
import { create } from 'zustand';
import type { Plan, StaticObject, Issue, Size } from '@planner/shared';

// аббревиатуры для id
const TYPE_ABBR: Record<string, string> = {
  workplace: 'wp',
  door: 'door',
  window: 'win',
  column: 'col',
  fire_extinguisher: 'fe',
  fire_alarm: 'fa',
  electrical_shield: 'esh',
  comms_block: 'com',
  net_cabinet: 'net',
  cabinet: 'cab',
  wall: 'wall',
};

type State = {
  plan: Plan;
  selectedId?: string;
  issues: Issue[];

  setPlan: (p: Plan) => void;
  setSelected: (id?: string) => void;
  setIssues: (i: Issue[]) => void;

  updateObject: (id: string, patch: Partial<StaticObject['rect']>) => void;
  updateRoom: (patch: Partial<Size>) => void;

  // создание / удаление
  addObject: (o: StaticObject) => void;
  deleteObject: (id: string) => void;
  deleteSelected: () => void;

  // режим размещения
  placingType?: string;
  setPlacingType: (type?: string) => void;

  // генерация последовательных id по типу
  nextIdForType: (type: string) => string;
  _idCounters: Record<string, number>;
};

// инициализация счётчиков по текущим объектам (просто количество каждого типа)
function initCounters(objects: StaticObject[]) {
  const counters: Record<string, number> = {};
  for (const o of objects) counters[o.type] = (counters[o.type] ?? 0) + 1;
  return counters;
}

export const usePlanStore = create<State>((set, get) => ({
  plan: {
    room: { W: 11000, H: 7000 },
    objects: [
      { id: 'door-1', type: 'door', rect: { X: 2000, Y: 6800, W: 900, H: 200 }, properties: [{ kind: 'status', value: 'evacuation' }], requiresWallAnchor: true },
      { id: 'win-1', type: 'window', rect: { X: 2000, Y: 0, W: 2000, H: 200 }, properties: [], requiresWallAnchor: true },
      { id: 'col-1', type: 'column', rect: { X: 3000, Y: 2500, W: 1000, H: 1000 }, properties: [] },
      { id: 'fe-1', type: 'fire_extinguisher', rect: { X: 500, Y: 500, W: 200, H: 200 }, properties: [] },
      { id: 'wp-1', type: 'workplace', rect: { X: 8000, Y: 3500, W: 1200, H: 800 }, properties: [] },
    ],
    task: { count: 10, size: { W: 1200, H: 800 } }
  },

  selectedId: undefined,
  issues: [],
  _idCounters: {},

  setPlan: (p) => set({ plan: p, _idCounters: initCounters(p.objects) }),
  setSelected: (id) => set({ selectedId: id }),
  setIssues: (i) => set({ issues: i }),

  updateObject: (id, patch) => set(s => ({
    plan: { ...s.plan, objects: s.plan.objects.map(o => o.id === id ? { ...o, rect: { ...o.rect, ...patch } } : o) }
  })),

  updateRoom: (patch) => set(s => ({ plan: { ...s.plan, room: { ...s.plan.room, ...patch } } })),

  // последовательные id вида "<abbr>-<n>"
  nextIdForType: (type: string) => {
    const abbr = TYPE_ABBR[type] ?? (type.slice(0, 3) || 'obj');
    const counters = get()._idCounters;
    const n = (counters[type] ?? 0) + 1;
    counters[type] = n;
    return `${abbr}-${n}`;
  },

  addObject: (o) => set(s => {
    // если прилетел объект без id — сгенерируем
    let obj = o;
    if (!obj.id) {
      const abbr = TYPE_ABBR[o.type] ?? (o.type.slice(0, 3) || 'obj');
      const n = (s._idCounters[o.type] ?? 0) + 1;
      s._idCounters[o.type] = n;
      obj = { ...o, id: `${abbr}-${n}` };
    } else {
      // синхронизируем счётчик, если id нашего формата и номер больше текущего
      const abbr = TYPE_ABBR[o.type] ?? (o.type.slice(0, 3) || 'obj');
      const m = new RegExp(`^${abbr}-(\\d+)$`).exec(obj.id);
      if (m) {
        const num = Number(m[1]);
        s._idCounters[o.type] = Math.max(s._idCounters[o.type] ?? 0, num);
      }
    }
    return { plan: { ...s.plan, objects: [...s.plan.objects, obj] }, selectedId: obj.id };
  }),

  placingType: undefined,
  setPlacingType: (type) => set({ placingType: type }),

  deleteObject: (id) => set(s => ({
    plan: { ...s.plan, objects: s.plan.objects.filter(o => o.id !== id) },
    selectedId: s.selectedId === id ? undefined : s.selectedId
  })),
  deleteSelected: () => {
    const id = get().selectedId;
    if (id) get().deleteObject(id);
  },
}));

// инициализация счётчиков при первом запуске
const s = usePlanStore.getState();
usePlanStore.setState({ _idCounters: initCounters(s.plan.objects) });
