import type { IdentityStore } from "@wasp.sh/auth-contract";
import type { MethodProviderName, WaspAuthRuntime } from "./types.js";
/**
 * Wasp's own auth records each method's identities under its own provider
 * name: `username`, `email`, `google`, ... The manifest (see `spec.ts`)
 * declares exactly the enabled methods, so `runtime.identities` has one store
 * per enabled method and none for a disabled one.
 */
export declare function identitiesOf(runtime: WaspAuthRuntime, method: MethodProviderName): IdentityStore | undefined;
/** Account-wide operations (`merge`) are the same on every store; any will do. */
export declare function anyIdentities(runtime: WaspAuthRuntime): IdentityStore;
