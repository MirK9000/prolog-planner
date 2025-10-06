import {
  ArrayMinSize,
  IsArray,
  IsBoolean,
  IsNumber,
  IsOptional,
  IsString,
  Min,
  ValidateNested,
} from 'class-validator';
import { Type } from 'class-transformer';

/**
 * DTO describing a plan size (width/height in millimeters).
 */
export class SizeDto {
  @IsNumber()
  @Min(100, { message: 'Width must be at least 100mm' })
  W!: number;

  @IsNumber()
  @Min(100, { message: 'Height must be at least 100mm' })
  H!: number;
}

/**
 * DTO describing a rectangle position/size.
 */
export class RectDto {
  @IsNumber()
  @Min(0, { message: 'X coordinate must be non-negative' })
  X!: number;

  @IsNumber()
  @Min(0, { message: 'Y coordinate must be non-negative' })
  Y!: number;

  @IsNumber()
  @Min(1, { message: 'Width must be at least 1mm' })
  W!: number;

  @IsNumber()
  @Min(1, { message: 'Height must be at least 1mm' })
  H!: number;
}

/**
 * DTO describing a generic property entry associated with an object.
 */
export class PropertyDto {
  @IsString()
  kind!: string;

  @IsOptional()
  value?: unknown;
}

/**
 * DTO describing a static object placed inside a room.
 */
export class StaticObjectDto {
  @IsString()
  id!: string;

  @IsString()
  type!: string;

  @ValidateNested()
  @Type(() => RectDto)
  rect!: RectDto;

  @IsArray()
  @ValidateNested({ each: true })
  @Type(() => PropertyDto)
  properties!: PropertyDto[];

  @IsOptional()
  @IsNumber()
  orientation?: number;

  @IsOptional()
  @IsBoolean()
  requiresWallAnchor?: boolean;
}

/**
 * DTO describing optional task specification for the plan.
 */
export class TaskSpecDto {
  @IsNumber()
  @Min(1, { message: 'Task count must be at least 1' })
  count!: number;

  @ValidateNested()
  @Type(() => SizeDto)
  size!: SizeDto;
}

/**
 * Top-level DTO used to validate payload for export endpoint.
 */
export class ExportPlanDto {
  @ValidateNested()
  @Type(() => SizeDto)
  room!: SizeDto;

  @IsArray()
  @ArrayMinSize(0)
  @ValidateNested({ each: true })
  @Type(() => StaticObjectDto)
  objects!: StaticObjectDto[];

  @IsOptional()
  @ValidateNested()
  @Type(() => TaskSpecDto)
  task?: TaskSpecDto;
}
