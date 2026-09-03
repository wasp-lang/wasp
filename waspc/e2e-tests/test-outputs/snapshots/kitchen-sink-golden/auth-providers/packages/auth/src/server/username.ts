import { hashPassword, verifyPassword } from "@wasp.sh/lib-auth/node";

import { getBody, json, type Route } from "./http.js";
import { namespaceFor } from "./namespaces.js";
import type { Ctx } from "./types.js";
import {
  createInvalidCredentialsError,
  rethrowPossibleAuthError,
  validateAndGetUserFields,
} from "./utils.js";
import {
  ensurePasswordIsPresent,
  ensureValidPassword,
  ensureValidUsername,
} from "./validation.js";

/** The username & password method: `/auth/username/{login,signup}`. */
export function usernameRoutes({ runtime, extensions }: Ctx): Route[] {
  const identities = () => runtime.identityNamespaces(namespaceFor("username"));

  return [
    {
      method: "POST",
      path: "/username/login",
      handler: async (req, res) => {
        const fields = getBody(req);
        ensureValidUsername(fields);
        ensurePasswordIsPresent(fields);
        const username = fields.username as string;

        const identity = await identities().find(username);
        if (!identity) {
          throw createInvalidCredentialsError();
        }
        try {
          const secrets = await identities().getSecrets(username);
          if (secrets === null || typeof secrets.hashedPassword !== "string") {
            throw createInvalidCredentialsError();
          }
          await verifyPassword(
            secrets.hashedPassword,
            fields.password as string,
          );
        } catch {
          throw createInvalidCredentialsError();
        }

        // The mint goes through the same `wasp-sessions` facet any adapter
        // gets; the app's login hooks fire inside it.
        const { sessionId } = await runtime.sessions.issue(
          { namespace: namespaceFor("username"), subjectId: username },
          { req },
        );
        json(res, 200, { sessionId });
      },
    },
    {
      method: "POST",
      path: "/username/signup",
      handler: async (req, res) => {
        const fields = getBody(req);
        ensureValidUsername(fields);
        ensurePasswordIsPresent(fields);
        ensureValidPassword(fields);

        try {
          // The facet's `create` is the signup choke point: the app's
          // onBeforeSignup veto, then the lazy userSignupFields getters, then
          // the atomic write, then onAfterSignup.
          await identities().create(
            fields.username as string,
            {
              secrets: {
                hashedPassword: await hashPassword(fields.password as string),
              },
            },
            (() =>
              validateAndGetUserFields(
                fields,
                extensions.userSignupFields?.username,
              )) as never,
            { req },
          );
        } catch (e) {
          rethrowPossibleAuthError(e);
        }
        json(res, 200, { success: true });
      },
    },
  ];
}
