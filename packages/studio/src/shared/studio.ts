// Loads `devframe` so the augmentation below can find it.
import type {} from "devframe";
import type { Data } from "./types.js";

export const STUDIO_NAMESPACE = "wasp-studio";

// Typed shared state, so `ctx.scope(STUDIO_NAMESPACE).rpc.sharedState("data")`
// returns the right type on both the node and browser sides.
declare module "devframe" {
  interface DevframeRpcSharedStates {
    "wasp-studio:data": { data: Data | null };
  }
}
