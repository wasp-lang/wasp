import type { ServerAuthAdapterFor } from "@wasp.sh/auth-contract";
import type { clerk as clerkSpecHelper } from "./spec.js";
/**
 * Clerk, expressed as a Wasp `AuthHandler`.
 *
 * This is about the smallest possible handler, and Clerk is by far the least
 * work to integrate: it contributes **no Prisma models and no routes**. It
 * only ever answers "whose request is this?", from Clerk's own session token,
 * which the client auth handler puts on every request. Wasp issues nothing for it.
 *
 * It is also the handler that shows why `signIn` is optional on the
 * contract. Clerk has **no server-side password login at all** -- password
 * verification lives on its Frontend API behind a browser-held `__client`
 * cookie, and its Backend API has no endpoint that turns credentials into a
 * session. So no other scheme can sign into Clerk, and Clerk verifies no
 * login of its own on the server: it authenticates and signs out, and stops
 * there.
 *
 * Secrets come from `runtime.env`, already validated against the env vars the
 * manifest declared -- the handler never reads `process.env` itself.
 */
export declare const createServerAuthHandler: ServerAuthAdapterFor<typeof clerkSpecHelper>;
