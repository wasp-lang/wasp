import type { WaspClientRuntime } from "@wasp.sh/auth-contract/client";
import type { WaspAuthClientOptions } from "./types.js";
export declare function setClientState(newRuntime: WaspClientRuntime, newOptions: WaspAuthClientOptions): void;
export declare function getClientRuntime(): WaspClientRuntime;
export declare function getClientOptions(): WaspAuthClientOptions;
