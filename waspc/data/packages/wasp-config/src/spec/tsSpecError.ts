export class TsSpecError extends Error {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options);
    this.name = "Wasp Spec Error";
  }
}
