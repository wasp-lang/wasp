import { verify } from "@node-rs/argon2";
import { getIdentityStore } from "wasp/server/auth";
import type {
  AuthProvider,
  AuthenticateResult,
} from "wasp/server/auth/provider/types";

/**
 * Email+password auth, hand-rolled in-app -- the proof that a user-made
 * provider has the same powers Wasp's own auth uses, byte for byte:
 *
 * - Credential storage: the identity store's `secrets` channel, in the column
 *   the Prisma client omits by default. Hashing is this file's explicit job
 *   (argon2, brought by this app -- Wasp ships no crypto to external
 *   providers).
 * - Sessions: none of our own. `authenticate` verifies a `Basic` credential
 *   once, at the `POST /auth/login` exchange, and Wasp mints its session from
 *   the result. A stateless verifier returns no `sessionId`, so logout has
 *   nothing to revoke upstream -- Wasp's session is the only one.
 */
export const identities = getIdentityStore("external:password");

export const passwordAuthProvider: AuthProvider = {
  id: "external:password",

  async authenticate(request: Request): Promise<AuthenticateResult> {
    const credentials = parseBasicAuthHeader(
      request.headers.get("authorization"),
    );
    if (credentials === null) {
      return { status: "unauthenticated" };
    }

    const email = normalizeEmail(credentials.email);
    const secrets = await identities.getSecrets(email);
    if (secrets === null || typeof secrets.hashedPassword !== "string") {
      return { status: "unauthenticated" };
    }

    const passwordMatches = await verify(
      secrets.hashedPassword,
      credentials.password,
    ).catch(() => false);
    if (!passwordMatches) {
      return { status: "unauthenticated" };
    }

    return {
      status: "authenticated",
      session: { subjectId: email, claims: { email } },
    };
  },
};

/**
 * The identity key. The store normalizes only Wasp's own provider names, so
 * casing discipline for a custom provider is the provider's job -- signup and
 * login must agree.
 */
export function normalizeEmail(email: string): string {
  return email.trim().toLowerCase();
}

function parseBasicAuthHeader(
  header: string | null,
): { email: string; password: string } | null {
  const prefix = "Basic ";
  if (header === null || !header.startsWith(prefix)) {
    return null;
  }
  const decoded = Buffer.from(
    header.substring(prefix.length),
    "base64",
  ).toString("utf8");
  const separatorIndex = decoded.indexOf(":");
  if (separatorIndex < 1) {
    return null;
  }
  return {
    email: decoded.substring(0, separatorIndex),
    password: decoded.substring(separatorIndex + 1),
  };
}
