import type { ProviderIdentities } from "@wasp.sh/auth-contract";

import type { MethodProviderName, WaspAuthRuntime } from "./types.js";

/**
 * Wasp's own auth records each method's identities in its own namespace
 * under the scheme's name: `<scheme>:username`, `<scheme>:email`,
 * `<scheme>:google`, ... The manifest (see `spec.ts`) declares exactly the
 * enabled methods as suffixes, so `runtime.identities` has one store per
 * enabled method and none for a disabled one.
 */
export function identitiesOf(
  runtime: WaspAuthRuntime,
  method: MethodProviderName,
): ProviderIdentities | undefined {
  return (runtime.identities as Partial<WaspAuthRuntime["identities"]>)[method];
}

/** Account-wide operations (`merge`) are the same on every store; any will do. */
export function anyIdentities(runtime: WaspAuthRuntime): ProviderIdentities {
  return Object.values(runtime.identities)[0];
}
