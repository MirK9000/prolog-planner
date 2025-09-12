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
    // Standard Classroom demo plan
    room: { W: 10000, H: 8000 },
    objects: [
      // door on the bottom wall for evacuation
      { id: 'door-1', type: 'door', rect: { X: 500, Y: 7800, W: 900, H: 200 }, properties: [{ kind: 'status', value: 'evacuation' }], requiresWallAnchor: true },
      // two windows on the top wall
      { id: 'win-1', type: 'window', rect: { X: 1000, Y: 0, W: 2000, H: 200 }, properties: [], requiresWallAnchor: true },
      { id: 'win-2', type: 'window', rect: { X: 5000, Y: 0, W: 3000, H: 200 }, properties: [], requiresWallAnchor: true },
      // interior column obstructing the space
      { id: 'col-1', type: 'column', rect: { X: 4500, Y: 3200, W: 800, H: 800 }, properties: [] },
      // equipment along the walls
      { id: 'fe-1', type: 'fire_extinguisher', rect: { X: 1700, Y: 7600, W: 200, H: 200 }, properties: [], requiresWallAnchor: true },
      { id: 'esh-1', type: 'electrical_shield', rect: { X: 9800, Y: 400, W: 200, H: 300 }, properties: [], requiresWallAnchor: true },
      { id: 'com-1', type: 'comms_block', rect: { X: 9700, Y: 7700, W: 300, H: 300 }, properties: [], requiresWallAnchor: true },
      { id: 'net-1', type: 'net_cabinet', rect: { X: 9400, Y: 2500, W: 600, H: 500 }, properties: [], requiresWallAnchor: true },
      { id: 'cab-1', type: 'cabinet', rect: { X: 0, Y: 2000, W: 500, H: 1500 }, properties: [], requiresWallAnchor: true },
    ],
    task: { count: 12, size: { W: 1200, H: 800 } }
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
