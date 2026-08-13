/**
 * In development a single process prints the client's logs, the server's logs
 * and Vite's own, so Wasp's server-side plugins tag what they have to say.
 */
const logPrefix = "\x1b[35m[server]\x1b[0m";

/**
 * We log through `console` instead of Vite's logger because the logger belongs
 * to a config object, and the config a plugin sees isn't always the one of the
 * dev server the user is looking at (`wasp:validate-env` resolves a silent
 * config of its own).
 */
// PRIVATE API (used by Wasp's `server` environment plugins)
export function log(message: string): void {
  console.log(`${logPrefix} ${message}`);
}

// PRIVATE API (used by Wasp's `server` environment plugins)
export function logError(message: string, error?: unknown): void {
  console.error(`${logPrefix} ${message}`);
  if (error !== undefined) {
    console.error(error);
  }
}
