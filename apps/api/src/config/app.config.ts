/**
 * Application configuration
 * Centralized place for runtime settings.
 */
export const appConfig = {
  /** Server port (default 3333) */
  port: parseInt(process.env.PORT || '3333', 10),
  /** Allowed CORS origin */
  corsOrigin: process.env.CORS_ORIGIN || '*',
  /** Node environment */
  nodeEnv: process.env.NODE_ENV || 'development',
  /** Log level threshold */
  logLevel: process.env.LOG_LEVEL || 'log',
} as const;

export type AppConfig = typeof appConfig;
