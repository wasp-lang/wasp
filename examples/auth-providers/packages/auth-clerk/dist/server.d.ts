import type { ServerAdapterFactory } from "@wasp.sh/auth-contract";
/**
 * Clerk, expressed as a Wasp `AuthProvider`.
 *
 * This is about the smallest possible adapter, and Clerk is by far the least
 * work to integrate: it contributes **no Prisma models and no routes**. It only
 * ever answers "whose request is this?".
 *
 * It is also the adapter that proves why session issuance is a separate
 * capability (`SupportsSessionIssuance`) rather than part of the base
 * interface. Clerk has **no
 * server-side password login at all** -- password verification lives on its
 * Frontend API behind a browser-held `__client` cookie, and its Backend API has
 * no endpoint that turns credentials into a session. So this adapter implements
 * `AuthProvider & SupportsSessionRevocation` and stops there: revocation yes, issuing no. A uniform `login(email, password)` could only
 * be implemented for Clerk as something that throws or silently ignores its
 * arguments; a missing capability is the honest alternative.
 *
 * Secrets come from `runtime.env`, already validated against the env vars the
 * manifest declared -- the adapter never reads `process.env` itself.
 */
export declare const createServerAdapter: ServerAdapterFactory;
