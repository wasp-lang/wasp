import { Router } from "express";

import { HttpError } from 'wasp/server';
import { handleRejection } from 'wasp/server/utils'
import { findAuthWithUserBy } from 'wasp/auth/utils'
import { createSession } from 'wasp/auth/session'
import { exchangeCodeForTokenPath, tokenStore } from "wasp/server/auth";

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
