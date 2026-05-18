/**
 * Use Result instead of exceptions when expected failures should be reported
 * without stack traces and kept visible in the type system.
 */
export type Result<Value, Error> =
  | { status: "ok"; value: Value }
  | { status: "error"; error: Error };
