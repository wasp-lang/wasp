import type { WaspClientRuntime } from "@wasp.sh/auth-contract/client";
import type { WaspAuthClientSpec } from "./types.js";
export declare function setClientState(newRuntime: WaspClientRuntime, newSpec: WaspAuthClientSpec): void;
export declare function getClientRuntime(): WaspClientRuntime;
export declare function getClientSpec(): WaspAuthClientSpec;
