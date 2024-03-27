import { Router } from "express";

import { HttpError } from 'wasp/server';
import { handleRejection } from 'wasp/server/utils'
import { createJWT, validateJWT, TimeSpan } from 'wasp/auth/jwt'
import { findAuthWithUserBy } from 'wasp/auth/utils'
import { createSession } from 'wasp/auth/session'
import { exchangeCodeForTokenPath } from "./redirect.js";

export const tokenStore = createTokenStore();

export function setupOneTimeCodeRoute(router: Router) {
  router.post(
    `/${exchangeCodeForTokenPath}`,
    handleRejection(async (req, res) => {
      const { code } = req.body;

      if (code === undefined) {
        throw new HttpError(400, "Unable to login with the OAuth provider. The code is missing.");
      }

      if (tokenStore.isUsed(code)) {
        throw new HttpError(400, "Unable to login with the OAuth provider. The code has already been used.");
      }

      const { id: authId } = await tokenStore.verifyToken(code);
      const auth = await findAuthWithUserBy({ id: authId })

      if (auth === null) {
        throw new HttpError(400, "Unable to login with the OAuth provider. The code is invalid.");
      }

      const session = await createSession(auth.id);

      tokenStore.markUsed(code);

      return res.json({
        sessionId: session.id,
      });
    })
  );
}

function createTokenStore() {
  const usedTokens = new Map<string, number>();

  const validFor = new TimeSpan(1, 'm') // 1 minute
  const cleanupAfter = 1000 * 60 * 60; // 1 hour

  function createToken(userId: string): Promise<string> {
    return createJWT(
      {
        id: userId,
      },
      {
        expiresIn: validFor,
      }
    );
  }

  function verifyToken(token: string): Promise<{ id: string }> {
    return validateJWT(token);
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
