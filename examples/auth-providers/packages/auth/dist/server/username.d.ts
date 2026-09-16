import { type Route } from "./http.js";
import type { Ctx } from "./types.js";
/** The username & password method: `/auth/username/{login,signup}`. */
export declare function usernameRoutes({ runtime, extensions }: Ctx): Route[];
