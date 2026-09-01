import { Router } from "express";

import { HttpError } from 'wasp/server';
import { defineHandler } from 'wasp/server/utils';
import { waspAuthRuntime } from 'wasp/server/auth/provider';
import { exchangeCodeForTokenPath, tokenStore } from "wasp/server/auth";

export function setupOneTimeCodeRoute(router: Router) {
  router.post(
    `/${exchangeCodeForTokenPath}`,
    defineHandler(async (req, res) => {
      const { code } = req.body;

      if (code === undefined) {
        throw new HttpError(400, "Unable to login with the OAuth provider. The code is missing.");
      }

      if (tokenStore.isUsed(code)) {
        throw new HttpError(400, "Unable to login with the OAuth provider. The code has already been used.");
      }

      const { namespace, subjectId } = await tokenStore.verifyToken(code);

      // Minting goes through the same `wasp-sessions` facet an adapter
      // package gets. `skipHooks`: the app's login hooks already fired at the
      // OAuth callback, where the tokens were available to pass to them.
      const { sessionId } = await waspAuthRuntime.sessions.issue(
        { namespace, subjectId },
        { req, skipHooks: true },
      ).catch(() => {
        throw new HttpError(400, "Unable to login with the OAuth provider. The code is invalid.");
      });

      tokenStore.markUsed(code);

      res.json({
        sessionId,
      });
    })
  );
}
