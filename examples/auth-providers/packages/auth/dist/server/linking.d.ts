import { type Route } from "./http.js";
import type { Ctx, Req } from "./types.js";
/**
 * The account of the signed-in user making this request. Goes through the
 * credentials facet, so it works whatever issuer this scheme signs into
 * (its private one, or a sibling `waspBearer()` / `waspCookie()`), bearer or
 * cookie. For a Wasp-issued credential the principal's subject IS the Auth id.
 */
export declare function requireCurrentAuthId({ runtime }: Ctx, req: Req): Promise<string>;
/** Maps the facet's link/unlink rejections onto HTTP answers the client reads. */
export declare function rethrowLinkError(e: unknown): never;
export type LinkTicket = {
    linkToAuthId: string;
};
/**
 * Routes every method shares: `/unlink`, and `/link-intent` for the OAuth
 * methods. An OAuth link starts with a browser NAVIGATION, which cannot carry
 * a bearer credential; the client first trades its credential for a
 * short-lived signed ticket here, and the navigation carries that instead.
 */
export declare function linkingRoutes(ctx: Ctx, hasOAuth: boolean): Route[];
