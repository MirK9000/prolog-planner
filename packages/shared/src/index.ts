
export type Mm = number;

export type ObjectType =
  | 'wall' | 'window' | 'door' | 'column' | 'electrical_shield' | 'comms_block'
  | 'fire_extinguisher' | 'fire_alarm' | 'net_cabinet' | 'cabinet' | 'panel'
  | 'teacher_zone' | 'workplace';

export interface Size { W: Mm; H: Mm }
export interface Rect { X: Mm; Y: Mm; W: Mm; H: Mm }

export type Property =
  | { kind: 'capacity'; value: number }
  | { kind: 'status'; value: 'evacuation' }
  | { kind: 'type'; value: string };

export interface StaticObject {
  id: string;
  type: ObjectType;
  rect: Rect;
  properties: Property[];
  requiresWallAnchor?: boolean;
}

export interface TaskSpec { count: number; size: Size }

export interface Plan {
  room: Size;
  objects: StaticObject[];
  task?: TaskSpec;
}

export type Severity = 'error' | 'warning' | 'info';
export interface Issue {
  code: string;
  severity: Severity;
  message: string;
  objectId?: string;
  where?: Rect;
  fix?: { label: string; apply: (plan: Plan) => Plan }[];
}
