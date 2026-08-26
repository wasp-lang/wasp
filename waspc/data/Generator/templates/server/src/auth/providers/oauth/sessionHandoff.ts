import { Router } from "express";

import { HttpError } from "wasp/server";
import {
  sessionHandoffCodes,
  sessionHandoffExchangePath,
} from "wasp/server/auth";
import { createSession } from "wasp/server/auth/session";
import { findAuthWithUserBy } from "wasp/server/auth/utils";
import { defineHandler } from "wasp/server/utils";

export function setupSessionHandoffExchangeRoute(router: Router) {
  router.post(
    `/${sessionHandoffExchangePath}`,
    defineHandler(async (req, res) => {
      const { sessionHandoffCode } = req.body;

      if (sessionHandoffCode === undefined) {
        throw new HttpError(
          400,
          "Unable to login with the OAuth provider. The session handoff code is missing.",
        );
      }

      let authId: string | null;
      try {
        authId = await sessionHandoffCodes.redeem(sessionHandoffCode);
      } catch {
        throw new HttpError(
          400,
          "Unable to login with the OAuth provider. The session handoff code is invalid.",
        );
      }

      if (authId === null) {
        throw new HttpError(
          400,
          "Unable to login with the OAuth provider. The session handoff code has already been redeemed.",
        );
      }

      const auth = await findAuthWithUserBy({ id: authId });

      if (auth === null) {
        throw new HttpError(
          400,
          "Unable to login with the OAuth provider. The session handoff code is invalid.",
        );
      }

      const session = await createSession(auth.id);

      res.json({
        sessionId: session.id,
      });
    }),
  );
}
