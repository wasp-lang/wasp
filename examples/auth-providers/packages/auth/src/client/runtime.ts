import type { WaspClientRuntime } from "@wasp.sh/auth-contract/client";

import type { WaspAuthClientOptions } from "./types.js";

/**
 * The client adapter's window into the app, captured when Wasp instantiates
 * the adapter. Forms and actions read it; nothing here imports generated
 * code.
 */
let runtime: WaspClientRuntime | null = null;
let options: WaspAuthClientOptions | null = null;

export function setClientState(
  newRuntime: WaspClientRuntime,
  newOptions: WaspAuthClientOptions,
): void {
  runtime = newRuntime;
  options = newOptions;
}

export function getClientRuntime(): WaspClientRuntime {
  if (runtime === null) {
    throw new Error(
      "Wasp's auth client used before Wasp instantiated it. Is waspAuth() among app.auth.providers?",
    );
  }
  return runtime;
}

export function getClientOptions(): WaspAuthClientOptions {
  if (options === null) {
    throw new Error(
      "Wasp's auth client used before Wasp instantiated it. Is waspAuth() among app.auth.providers?",
    );
  }
  return options;
}
