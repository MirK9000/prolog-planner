import {
  CallHandler,
  ExecutionContext,
  Injectable,
  Logger,
  NestInterceptor,
} from '@nestjs/common';
import { Observable } from 'rxjs';
import { tap } from 'rxjs/operators';

/**
 * Logs every HTTP request duration and success/error state.
 */
@Injectable()
export class LoggingInterceptor implements NestInterceptor {
  private readonly logger = new Logger(LoggingInterceptor.name);

  intercept(context: ExecutionContext, next: CallHandler): Observable<unknown> {
    const request = context.switchToHttp().getRequest();
    const { method, url } = request;
    const started = Date.now();

    return next.handle().pipe(
      tap({
        next: () => {
          const elapsed = Date.now() - started;
          this.logger.log(`${method} ${url} - ${elapsed}ms - SUCCESS`);
        },
        error: (error: Error) => {
          const elapsed = Date.now() - started;
          this.logger.error(
            `${method} ${url} - ${elapsed}ms - ERROR: ${error.message}`,
          );
        },
      }),
    );
  }
}
