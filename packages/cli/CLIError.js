// @ts-check

/*
This Error wrapper exists to ensure a consistent human-friendly error reporting
experience for errors that occur in the CLI.

In general, errors in this wrapper only happen when something is wrong in the
actual process of finding and launching the CLI executable. As such, errors that
happen inside of the Wasp CLI process itself are not thrown inside this wrapper,
we just let the executable print whatever it needs and mimic its exit code.
*/
export class CLIError extends Error {
  static rethrowWith =
    (/** @type {string} */ message) => (/** @type {any} */ cause) => {
      if (cause instanceof CLIError) {
        // This is already a CLIError with a proper message, so just rethrow it.
        throw cause;
      } else {
        // Wrap the original error into a new CLIError with the provided message.
        throw new CLIError(message, { cause });
      }
    };

  static log = (/** @type {any} */ error) => {
    if (error instanceof CLIError) {
      console.error("Error:", error.message);
    } else {
      console.error(
        "Unknown Error launching Wasp:",
        error.message || String(error),
      );
    }

    if (error.cause) {
      console.error("Caused by:", error.cause.message || String(error.cause));
    }
  };
}
