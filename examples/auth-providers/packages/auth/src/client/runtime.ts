import type { WaspClientRuntime } from "@wasp.sh/auth-contract/client";

import type { WaspAuthClientConfig } from "./types.js";

/**
 * The client auth handler's window into the app, captured when Wasp instantiates
 * the handler. Forms and actions read it; nothing here imports generated
 * code.
 */
let runtime: WaspClientRuntime | null = null;
let config: WaspAuthClientConfig | null = null;

export function setClientState(
  newRuntime: WaspClientRuntime,
  newConfig: WaspAuthClientConfig,
): void {
  runtime = newRuntime;
  config = newConfig;
}

export function getClientRuntime(): WaspClientRuntime {
  if (runtime === null) {
    throw new Error(
      "Wasp's auth client used before Wasp instantiated it. Is waspAuth() among app.auth.schemes?",
    );
  }
  return runtime;
}

export function getClientConfig(): WaspAuthClientConfig {
  if (config === null) {
    throw new Error(
      "Wasp's auth client used before Wasp instantiated it. Is waspAuth() among app.auth.schemes?",
    );
  }
  return config;
}
