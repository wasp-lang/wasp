import { defineHandler } from "@wasp.sh/lib-sdk-core/node";
import { Router, type RequestHandler } from "express";
import { createInvalidCredentialsError } from "../errors.js";
import type { RequestWithAuth } from "./middleware.js";
import type { ProviderConfig } from "./providers.js";
import type { Sessions } from "./sessions.js";

/**
 * Creates the `/auth/me` handler. It expects the auth middleware to have run.
 *
 * @param serialize The app's response serializer (superjson with the app's
 * registered custom types).
 */
export function createMeHandler(
  serialize: (value: unknown) => unknown,
): RequestHandler {
  return defineHandler(async (req, res) => {
    const { user } = req as RequestWithAuth<unknown>;
    if (user) {
      res.json(serialize(user));
    } else {
      res.json(serialize(null));
    }
  });
}

/** Creates the `/auth/logout` handler. It expects the auth middleware to have run. */
export function createLogoutHandler(
  sessions: Sessions<unknown>,
): RequestHandler {
  return defineHandler(async (req, res) => {
    const { sessionId } = req as RequestWithAuth<unknown>;
    if (sessionId) {
      await sessions.invalidateSession(sessionId);
      res.json({ success: true });
    } else {
      throw createInvalidCredentialsError();
    }
  });
}

/**
 * Mounts each enabled provider's router under `/<provider id>`.
 *
 * @param setupSharedRoutes Adds routes shared by several providers (e.g. the
 * OAuth one-time code exchange) before the provider routers are mounted.
 */
export function createAuthProvidersRouter(
  providers: ProviderConfig[],
  setupSharedRoutes?: (router: Router) => void,
): Router {
  const router = Router();

  setupSharedRoutes?.(router);

  for (const provider of providers) {
    const { createRouter } = provider;
    const providerRouter = createRouter(provider);
    router.use(`/${provider.id}`, providerRouter);
    console.log(`🚀 "${provider.displayName}" auth initialized`);
  }

  return router;
}
