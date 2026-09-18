import type { WaspClientRuntime } from "@wasp.sh/auth-contract/client";
import type { WaspAuthClientConfig } from "./types.js";
export declare function setClientState(newRuntime: WaspClientRuntime, newConfig: WaspAuthClientConfig): void;
export declare function getClientRuntime(): WaspClientRuntime;
export declare function getClientConfig(): WaspAuthClientConfig;
