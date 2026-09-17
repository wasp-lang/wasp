import { hashPassword, verifyPassword } from "@wasp.sh/lib-auth/node";

import {
  getBody,
  getSignInProperties,
  json,
  sendAuthResponse,
  type Route,
} from "./http.js";
import { requireCurrentAuthId, rethrowLinkError } from "./linking.js";
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
  normalizeUsername,
} from "./validation.js";

/** The username & password method: `/auth/username/{login,signup}`. */
export function usernameRoutes(ctx: Ctx): Route[] {
  const { runtime, extensions } = ctx;
  const identities = () =>
    runtime.identityNamespaces(namespaceFor(runtime, "username"));

  return [
    {
      method: "POST",
      path: "/username/login",
      handler: async (req, res) => {
        const fields = getBody(req);
        ensureValidUsername(fields);
        ensurePasswordIsPresent(fields);
        const username = normalizeUsername(fields.username as string);

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

        // The sign-in goes through the credentials facet any handler gets;
        // the app's login hooks fire inside it, and the credentials scheme
        // decides what the client receives.
        const { response } = await runtime.credentials.signIn(
          { namespace: namespaceFor(runtime, "username"), subjectId: username },
          { req, properties: getSignInProperties(fields) },
        );
        sendAuthResponse(res, response);
      },
    },
    {
      // Account linking: a username and password for the signed-in user.
      method: "POST",
      path: "/username/link",
      handler: async (req, res) => {
        const authId = await requireCurrentAuthId(ctx, req);
        const fields = getBody(req);
        ensureValidUsername(fields);
        ensurePasswordIsPresent(fields);
        ensureValidPassword(fields);
        try {
          // `link`, not `create`: no new user, no userSignupFields; the
          // app's link hooks fire inside.
          await identities().link(
            normalizeUsername(fields.username as string),
            {
              secrets: {
                hashedPassword: await hashPassword(fields.password as string),
              },
            },
            { authId, req },
          );
        } catch (e) {
          rethrowLinkError(e);
        }
        json(res, 200, { success: true });
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
            normalizeUsername(fields.username as string),
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
