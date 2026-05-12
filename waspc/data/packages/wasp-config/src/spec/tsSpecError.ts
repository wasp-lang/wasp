export class TsSpecError extends Error {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options);
    this.name = "Wasp Spec Error";
    // We want to hide the stack trace for user-facing errors.
    this.stack = undefined;
  }
}
