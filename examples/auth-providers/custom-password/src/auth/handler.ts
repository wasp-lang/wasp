import { getSchemeRuntime } from "wasp/server/auth";
import type { AuthHandler } from "wasp/server/auth/handler/types";

/**
 * Email+password auth, hand-rolled in-app -- the proof that a user-made
 * scheme has the same powers Wasp's own auth uses, byte for byte:
 *
 * - Credential storage: the identity store's `secrets` channel, in the column
 *   the Prisma client omits by default. Hashing is this app's explicit job
 *   (argon2, brought by this app -- Wasp ships no crypto to handlers).
 * - Credentials: the scheme's manifest declares `credentials: {}`, so Wasp
 *   runs a private bearer issuer for it (a token whose record lives in the
 *   `Session` table). The login route (`loginApi.ts`) verifies the password
 *   and signs the subject in through `runtime.credentials`; afterwards the
 *   request carries the issued token, and this handler recognizes it by
 *   forwarding `authenticate` to the issuer -- the way ASP.NET's remote
 *   schemes forward to their sign-in scheme.
 */
export const SCHEME = "password";

/** The scheme's runtime window: identities and the credentials facet. */
export function runtime() {
  return getSchemeRuntime<never, true>(SCHEME);
}

export const passwordAuthHandler: AuthHandler = {
  authenticate: (request) => runtime().credentials.authenticate(request),
  signOut: (request) => runtime().credentials.signOut(request),
};

/**
 * The identity key. The store normalizes only Wasp's own scheme names, so
 * casing discipline for a custom scheme is the scheme's job -- signup and
 * login must agree.
 */
export function normalizeEmail(email: string): string {
  return email.trim().toLowerCase();
}
