import { type Route } from "../http.js";
import type { Ctx } from "../types.js";
/** The email method: `/auth/email/{signup,login,verify-email,request-password-reset,reset-password}`. */
export declare function emailRoutes({ runtime, options, extensions }: Ctx): Route[];
/**
 * The business user behind an auth id, for the method-specific hooks' `user`
 * parameter. Through the app's PrismaClient: the `Auth` model and its `user`
 * relation are fixed by Wasp's schema injection, whatever the app calls its
 * user entity.
 */
export declare function findAuthWithUser(runtime: Ctx["runtime"], authId: string): Promise<{
    id: string;
    user: unknown;
} | null>;
