export class WaspHttpError extends Error {
  statusCode: number;

  data: unknown;

  constructor(statusCode: number, message: string, data: unknown) {
    super(message);
    this.statusCode = statusCode;
    this.data = data;
  }
}
