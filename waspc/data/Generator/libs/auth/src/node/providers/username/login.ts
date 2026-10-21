import { defineHandler } from "@wasp.sh/lib-sdk-core/node";
import { createInvalidCredentialsError } from "../../../errors.js";
import {
  createProviderId,
  getProviderDataWithPassword,
} from "../../../providerData.js";
import {
  ensurePasswordIsPresent,
  ensureValidUsername,
} from "../../../validation.js";
import type { AuthHookTypes } from "../../hooks.js";
import { verifyPassword } from "../../password.js";
import type { UsernameAuthDependencies } from "./provider.js";

export function getLoginRoute<H extends AuthHookTypes>({
  identities,
  sessions,
  hooks,
}: UsernameAuthDependencies<H>) {
  return defineHandler(async (req, res) => {
    const fields = req.body ?? {};
    ensureValidArgs(fields);

    const providerId = createProviderId("username", fields.username);
    const authIdentity = await identities.findAuthIdentity(providerId);
    if (!authIdentity) {
      throw createInvalidCredentialsError();
    }

    try {
      const providerData = getProviderDataWithPassword<"username">(
        authIdentity.providerData,
      );

      await verifyPassword(providerData.hashedPassword, fields.password);
    } catch (e) {
      throw createInvalidCredentialsError();
    }

    const auth = await identities.findAuthWithUserById(authIdentity.authId);

    if (auth === null) {
      throw createInvalidCredentialsError();
    }

    await hooks.onBeforeLoginHook({
      req,
      providerId,
      user: auth.user,
    });

    const session = await sessions.createSession(auth.auth.id);

    await hooks.onAfterLoginHook({
      req,
      providerId,
      user: auth.user,
    });

    res.json({
      sessionId: session.id,
    });
  });
}

function ensureValidArgs(args: object): void {
  ensureValidUsername(args);
  ensurePasswordIsPresent(args);
}
