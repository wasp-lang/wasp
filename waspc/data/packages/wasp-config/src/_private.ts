import type { TsModuleSpec } from "./publicApi/tsAppSpec.js";

export const GET_TS_APP_SPEC = Symbol("GET_TS_APP_SPEC");

// Uses Symbol.for (global registry) instead of Symbol() so the key is
// accessible across packages that each bundle their own copy of this module.
const MODULE_SPEC_KEY = Symbol.for("wasp:module-spec");

export function getModuleSpec(module: unknown): TsModuleSpec {
  return (module as any)[MODULE_SPEC_KEY];
}

export function setModuleSpec(module: unknown, spec: TsModuleSpec): void {
  (module as any)[MODULE_SPEC_KEY] = spec;
}
