import type { Request, Response } from "express";
import { createInvalidCredentialsError } from "../../../errors.js";
import {
  createProviderId,
  getProviderDataWithPassword,
} from "../../../providerData.js";
import {
  ensurePasswordIsPresent,
  ensureValidEmail,
} from "../../../validation.js";
import type { AuthHookTypes } from "../../hooks.js";
import { verifyPassword } from "../../password.js";
import type { EmailAuthDependencies } from "./provider.js";

export function getLoginRoute<H extends AuthHookTypes>({
  identities,
  sessions,
  hooks,
}: EmailAuthDependencies<H>) {
  return async function login(req: Request, res: Response): Promise<void> {
    const fields = req.body ?? {};
    ensureValidArgs(fields);

    const providerId = createProviderId("email", fields.email);
    const authIdentity = await identities.findAuthIdentity(providerId);
    if (!authIdentity) {
      throw createInvalidCredentialsError();
    }
    const providerData = getProviderDataWithPassword<"email">(
      authIdentity.providerData,
    );
    if (!providerData.isEmailVerified) {
      throw createInvalidCredentialsError();
    }
    try {
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
  };
}

function ensureValidArgs(args: object): void {
  ensureValidEmail(args);
  ensurePasswordIsPresent(args);
}
