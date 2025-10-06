declare module 'express' {
  export interface Request {
    url: string;
    method: string;
    [key: string]: unknown;
  }

  export interface Response {
    status(code: number): Response;
    json(body: unknown): Response;
  }
}
