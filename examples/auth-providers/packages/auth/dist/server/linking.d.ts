import { type Route } from "./http.js";
import type { Ctx, Req } from "./types.js";
/**
 * The account of the signed-in user making this request, as Wasp sees it:
 * whatever issuer this scheme signs into, bearer or cookie.
 */
export declare function requireCurrentAuthId({ runtime }: Ctx, req: Req): Promise<string>;
/** Maps the facet's link/unlink rejections onto HTTP answers the client reads. */
export declare function rethrowLinkError(e: unknown): never;
export type LinkTicket = {
    linkToAuthId: string;
};
/** Names both accounts of a pending merge; only `intoAuthId` may redeem it. */
export type MergeTicket = {
    fromAuthId: string;
    intoAuthId: string;
};
/**
 * The link failed because the login belongs to another account. When the app
 * turned merging on AND the caller has just proven control of that login,
 * answer "merge required" with a signed ticket instead; the client confirms
 * with the user and posts it to `/merge`.
 *
 * `proveControl` is what makes this safe: without it any signed-in user
 * could absorb any account by naming its login. An unproven attempt falls
 * through to the ordinary "linked elsewhere" answer, so this is no oracle
 * for guessing another account's password.
 */
export declare function offerMergeOrRethrow(ctx: Ctx, e: unknown, attempt: {
    intoAuthId: string;
    /** The existing identity's account, or null when it cannot be found. */
    findFromAuthId: () => Promise<string | null>;
    proveControl: () => Promise<boolean>;
}): Promise<never>;
export declare function createMergeTicket({ runtime }: Ctx, ticket: MergeTicket): Promise<string>;
/**
 * Routes every method shares: `/unlink`, and `/link-intent` for the OAuth
 * methods. An OAuth link starts with a browser NAVIGATION, which cannot carry
 * a bearer credential; the client first trades its credential for a one-time
 * code here, and the navigation carries that instead. Wasp issues the code,
 * and none under a cookie credential, which a navigation carries by itself,
 * so nothing here knows the transport.
 */
export declare function linkingRoutes(ctx: Ctx, hasOAuth: boolean): Route[];
