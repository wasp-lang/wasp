import type { WaspClientRuntime } from "@wasp.sh/auth-contract/client";

import type { WaspAuthClientSpec } from "./types.js";

/**
 * The client auth handler's window into the app, captured when Wasp instantiates
 * the handler. Forms and actions read it; nothing here imports generated
 * code.
 */
let runtime: WaspClientRuntime | null = null;
let spec: WaspAuthClientSpec | null = null;

export function setClientState(
  newRuntime: WaspClientRuntime,
  newSpec: WaspAuthClientSpec,
): void {
  runtime = newRuntime;
  spec = newSpec;
}

export function getClientRuntime(): WaspClientRuntime {
  if (runtime === null) {
    throw new Error(
      "Wasp's auth client used before Wasp instantiated it. Is waspAuth() among app.auth.schemes?",
    );
  }
  return runtime;
}

export function getClientSpec(): WaspAuthClientSpec {
  if (spec === null) {
    throw new Error(
      "Wasp's auth client used before Wasp instantiated it. Is waspAuth() among app.auth.schemes?",
    );
  }
  return spec;
}
