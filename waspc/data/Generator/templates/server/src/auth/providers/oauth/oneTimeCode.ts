import { Router } from "express";

import { exchangeOAuthCodeForSession } from "@wasp.sh/lib-auth/node";
import { createSession } from "wasp/auth/session";
import {
  findAuthWithUserBy,
  rethrowPossibleAuthServiceError,
} from "wasp/auth/utils";
import { exchangeCodeForTokenPath, tokenStore } from "wasp/server/auth";
import { defineHandler } from "wasp/server/utils";

export function setupOneTimeCodeRoute(router: Router) {
  router.post(
    `/${exchangeCodeForTokenPath}`,
    defineHandler(async (req, res) => {
      try {
        const { sessionId } = await exchangeOAuthCodeForSession({
          fields: req.body ?? {},
          adapters: {
            oneTimeCodeStore: {
              isUsed: tokenStore.isUsed,
              markUsed: tokenStore.markUsed,
              async verifyToken(code) {
                const { id: authId } = await tokenStore.verifyToken(code);
                return { authId };
              },
            },
            authRepository: {
              async findAuthWithUserByAuthId(authId) {
                const auth = await findAuthWithUserBy({ id: authId });
                return auth === null
                  ? null
                  : { authId: auth.id, user: auth.user };
              },
            },
            sessionService: {
              createSession,
            },
          },
        });

        res.json({
          sessionId,
        });
      } catch (e: unknown) {
        rethrowPossibleAuthServiceError(e);
        throw e;
      }
    }),
  );
}
