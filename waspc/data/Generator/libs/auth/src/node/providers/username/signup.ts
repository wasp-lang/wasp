import { defineHandler } from "@wasp.sh/lib-sdk-core/node";
import { createProviderId } from "../../../shared/providerData.js";
import {
  ensurePasswordIsPresent,
  ensureValidPassword,
  ensureValidUsername,
} from "../../../shared/validation.js";
import type { AuthHookTypes } from "../../hooks.js";
import type { UserSignupFields } from "../../identities.js";
import { sanitizeAndSerializeProviderData } from "../../providerDataStorage.js";
import type { UsernameAuthDependencies } from "./provider.js";

export function getSignupRoute<H extends AuthHookTypes>(
  {
    userSignupFields,
  }: {
    userSignupFields?: UserSignupFields;
  },
  { identities, hooks }: UsernameAuthDependencies<H>,
) {
  return defineHandler(async function signup(req, res) {
    const fields = req.body ?? {};
    ensureValidArgs(fields);

    const providerId = createProviderId("username", fields.username);

    // The hook runs first so it can veto the signup (by throwing) before the
    // developer's `userSignupFields` getters run.
    try {
      await hooks.onBeforeSignupHook({ req, providerId });
    } catch (e: unknown) {
      identities.rethrowPossibleAuthError(e);
    }

    const userFields = await identities.validateAndGetUserFields(
      fields,
      userSignupFields,
    );

    const providerData = await sanitizeAndSerializeProviderData<"username">({
      hashedPassword: fields.password,
    });

    try {
      const user = await identities.createUser(
        providerId,
        providerData,
        // Using any here because we want to avoid TypeScript errors and
        // rely on Prisma to validate the data.
        userFields as any,
      );
      await hooks.onAfterSignupHook({ req, providerId, user });
    } catch (e: unknown) {
      identities.rethrowPossibleAuthError(e);
    }

    res.json({ success: true });
  });
}

function ensureValidArgs(args: object): void {
  ensureValidUsername(args);
  ensurePasswordIsPresent(args);
  ensureValidPassword(args);
}
