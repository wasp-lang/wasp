/**
 * Error caused by invalid user input (unknown entities, bad imports, etc.).
 *
 * These are the errors users hit most often during development, so their
 * messages should be short, clear, and actionable.
 */
export class SpecUserError extends Error {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options);
    this.name = "Spec User Error";
    // Supress the stack trace since it isn't useful for user-facing problems.
    this.stack = undefined;
  }
}
