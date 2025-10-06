export interface DeskPlacement {
  id: string;
  type: string;
  rect: { x: number; y: number; w: number; h: number };
  orientation?: number;
  base_size?: { w: number; h: number };
}

export interface SolverSolution {
  grid?: number;
  desks: DeskPlacement[];
}

export interface SolverMetadata {
  executionTime: number;
  placedDesks: number;
  requestedDesks: number;
}

export interface SolverError {
  code: string;
  message: string;
  details?: { [key: string]: unknown };
}

export interface SolutionDto {
  success: boolean;
  solution?: SolverSolution;
  metadata?: SolverMetadata;
  error?: SolverError;
}
