import { HttpError } from "@wasp.sh/lib-sdk-core";
import { defineHandler } from "@wasp.sh/lib-sdk-core/node";
import type { Router } from "express";
import type { AuthEntityTypes } from "../../../shared/entities.js";
import type { AuthIdentities } from "../../identities.js";
import { TimeSpan, type JwtHelpers } from "../../jwt.js";
import type { Sessions } from "../../sessions.js";

export type OneTimeCodeStore = {
  createToken: (userId: string) => Promise<string>;
  verifyToken: (token: string) => Promise<{ id: string }>;
  isUsed: (token: string) => boolean;
  markUsed: (token: string) => void;
};

/**
 * Creates the store of the one-time codes the OAuth callback hands to the
 * client, which exchanges them for a session.
 */
export function createOneTimeCodeStore(jwt: JwtHelpers): OneTimeCodeStore {
  const usedTokens = new Map<string, number>();

  const validFor = new TimeSpan(1, "m"); // 1 minute
  const cleanupAfter = 1000 * 60 * 60; // 1 hour

  function createToken(userId: string): Promise<string> {
    return jwt.createJWT(
      {
        id: userId,
      },
      {
        expiresIn: validFor,
      },
    );
  }

  function verifyToken(token: string): Promise<{ id: string }> {
    return jwt.validateJWT(token);
  }

  function isUsed(token: string): boolean {
    return usedTokens.has(token);
  }

  function markUsed(token: string): void {
    usedTokens.set(token, Date.now());
    cleanUp();
  }

  function cleanUp(): void {
    const now = Date.now();
    for (const [token, timestamp] of usedTokens.entries()) {
      if (now - timestamp > cleanupAfter) {
        usedTokens.delete(token);
      }
    }
  }

  return {
    createToken,
    verifyToken,
    isUsed,
    markUsed,
  };
}

export type OneTimeCodeRouteDependencies<T extends AuthEntityTypes> = {
  exchangeCodeForTokenPath: string;
  tokenStore: OneTimeCodeStore;
  identities: Pick<AuthIdentities<T>, "findAuthWithUserById">;
  sessions: Sessions<unknown>;
};

/** Adds the route all OAuth providers share to exchange a one-time code for a session. */
export function setupOneTimeCodeRoute<T extends AuthEntityTypes>(
  router: Router,
  {
    exchangeCodeForTokenPath,
    tokenStore,
    identities,
    sessions,
  }: OneTimeCodeRouteDependencies<T>,
): void {
  router.post(
    `/${exchangeCodeForTokenPath}`,
    defineHandler(async (req, res) => {
      const { code } = req.body;

      if (code === undefined) {
        throw new HttpError(
          400,
          "Unable to login with the OAuth provider. The code is missing.",
        );
      }

      if (tokenStore.isUsed(code)) {
        throw new HttpError(
          400,
          "Unable to login with the OAuth provider. The code has already been used.",
        );
      }

      const { id: authId } = await tokenStore.verifyToken(code);
      const auth = await identities.findAuthWithUserById(authId);

      if (auth === null) {
        throw new HttpError(
          400,
          "Unable to login with the OAuth provider. The code is invalid.",
        );
      }

      const session = await sessions.createSession(auth.auth.id);

      tokenStore.markUsed(code);

      res.json({
        sessionId: session.id,
      });
    }),
  );
}
