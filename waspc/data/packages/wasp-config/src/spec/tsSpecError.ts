export class TsSpecError extends Error {
  static readonly prefix = "Wasp Spec Error";

  constructor(message: string, options?: ErrorOptions) {
    super(`${TsSpecError.prefix}: ${message}`, options);
    this.name = "TsSpecError";
  }
}
