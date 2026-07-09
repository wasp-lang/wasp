/**
 * Error caused by invalid user input (unknown entities, bad imports, etc.).
 *
 * Wasp's spec pipeline throws `WaspSpecUserError` when it finds a problem with the
 * user's configuration. Its message is reported to the user as a clean,
 * actionable error instead of an internal stack trace.
 *
 * Throw it from your own spec helper libraries when you want to report a
 * configuration problem the same way Wasp does:
 *
 * @example
 * ```ts
 * import { WaspSpecUserError } from "@wasp.sh/spec"
 *
 * export function requireEnv(name: string): string {
 *   const value = process.env[name]
 *   if (value === undefined) {
 *     throw new WaspSpecUserError(`Missing required environment variable '${name}'.`)
 *   }
 *   return value
 * }
 * ```
 *
 * These are the errors users hit most often during development, so keep their
 * messages short, clear, and actionable.
 *
 * @category Errors
 */
export class WaspSpecUserError extends Error {
  // Set so the error prints as `WaspSpecUserError: ...` instead of `Error: ...`.
  // `@internal` keeps it out of the generated API docs (see `typedoc.jsonc`).
  /** @internal */
  public override name = "WaspSpecUserError";
}
