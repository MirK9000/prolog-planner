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
    // Default room plan
    room: { W: 10000, H: 7000 },
    objects: [
      { id: 'cab-2', type: 'cabinet', rect: { X: 3900, Y: 6400, W: 1400, H: 600 }, properties: [], requiresWallAnchor: true },
      { id: 'cab-3', type: 'cabinet', rect: { X: 5500, Y: 6400, W: 1300, H: 600 }, properties: [], requiresWallAnchor: true },
      { id: 'col-2', type: 'column', rect: { X: 2300, Y: 1800, W: 1000, H: 1000 }, properties: [] },
      { id: 'col-3', type: 'column', rect: { X: 6700, Y: 1800, W: 1000, H: 1000 }, properties: [] },
      { id: 'door-2', type: 'door', rect: { X: 1500, Y: 6800, W: 1200, H: 200 }, properties: [], requiresWallAnchor: true },
      { id: 'esh-2', type: 'electrical_shield', rect: { X: 7300, Y: 6800, W: 800, H: 200 }, properties: [], requiresWallAnchor: true },
      { id: 'fa-1', type: 'fire_alarm', rect: { X: 900, Y: 6800, W: 200, H: 200 }, properties: [], requiresWallAnchor: true },
      { id: 'fe-2', type: 'fire_extinguisher', rect: { X: 200, Y: 6500, W: 300, H: 300 }, properties: [] },
      { id: 'fe-3', type: 'fire_extinguisher', rect: { X: 9500, Y: 6600, W: 300, H: 300 }, properties: [] },
      { id: 'net-2', type: 'net_cabinet', rect: { X: 8300, Y: 6400, W: 800, H: 600 }, properties: [], requiresWallAnchor: true },
      { id: 'win-3', type: 'window', rect: { X: 1600, Y: 0, W: 2000, H: 200 }, properties: [], requiresWallAnchor: true },
      { id: 'win-4', type: 'window', rect: { X: 4100, Y: 0, W: 2000, H: 200 }, properties: [], requiresWallAnchor: true },
      { id: 'win-5', type: 'window', rect: { X: 6600, Y: 0, W: 2000, H: 200 }, properties: [], requiresWallAnchor: true },
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
