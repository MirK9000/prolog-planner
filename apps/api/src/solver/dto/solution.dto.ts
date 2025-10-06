/**
 * Describes desk placement returned by solver.
 */
export interface DeskPlacementRectDto {
  x: number;
  y: number;
  w: number;
  h: number;
}

export interface DeskBaseSizeDto {
  w: number;
  h: number;
}

export interface DeskPlacementDto {
  id: string;
  type: string;
  rect: DeskPlacementRectDto;
  orientation?: number;
  base_size?: DeskBaseSizeDto;
}

export interface SolverMetadataDto {
  executionTime: number;
  placedDesks: number;
  requestedDesks: number;
}

export interface SolverErrorDto {
  code: string;
  message: string;
  details?: Record<string, unknown> | undefined;
}

export interface SolverSolutionPayload {
  grid?: number;
  desks: DeskPlacementDto[];
}

/**
 * Generic solver response payload.
 */
export interface SolutionDto {
  success: boolean;
  solution?: SolverSolutionPayload;
  metadata?: SolverMetadataDto;
  error?: SolverErrorDto;
}
