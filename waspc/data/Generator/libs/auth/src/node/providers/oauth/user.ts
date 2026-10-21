import type { Request as ExpressRequest } from "express";
import { createProviderId, type ProviderId } from "../../../shared/providerData.js";
import type { AuthHookTypes } from "../../hooks.js";
import type { UserSignupFields } from "../../identities.js";
import { sanitizeAndSerializeProviderData } from "../../providerDataStorage.js";
import type { ProviderConfig } from "../../providers.js";
import type { OAuthDependencies } from "./handler.js";

export async function finishOAuthFlowAndGetRedirectUri<H extends AuthHookTypes>(
  {
    provider,
    providerProfile,
    providerUserId,
    userSignupFields,
    req,
    oauth,
  }: {
    provider: ProviderConfig;
    providerProfile: unknown;
    providerUserId: string;
    userSignupFields: UserSignupFields | undefined;
    req: ExpressRequest;
    oauth: H["oauthData"];
  },
  dependencies: OAuthDependencies<H>,
): Promise<URL> {
  const providerId = createProviderId(provider.id, providerUserId);

  const authId = await getAuthIdFromProviderDetails(
    {
      providerId,
      providerProfile,
      userSignupFields,
      req,
      oauth,
    },
    dependencies,
  );

  const oneTimeCode = await dependencies.tokenStore.createToken(authId);

  return dependencies.redirects.getRedirectUriForOneTimeCode(oneTimeCode);
}

// We need a user id to create the auth token, so we either find an existing user
// or create a new one if none exists for this provider.
async function getAuthIdFromProviderDetails<H extends AuthHookTypes>(
  {
    providerId,
    providerProfile,
    userSignupFields,
    req,
    oauth,
  }: {
    providerId: ProviderId;
    providerProfile: any;
    userSignupFields: UserSignupFields | undefined;
    req: ExpressRequest;
    oauth: H["oauthData"];
  },
  { identities, hooks }: OAuthDependencies<H>,
): Promise<string> {
  const existingAuthIdentity = await identities.findAuthIdentity(providerId);

  if (existingAuthIdentity) {
    const authId = existingAuthIdentity.authId;

    // NOTE: Fetching the user to pass it to the login hooks - it's a bit wasteful
    // but we wanted to keep the onAfterLoginHook params consistent for all auth providers.
    const auth = await identities.findAuthWithUserById(authId);

    if (auth === null) {
      throw new Error(
        "Auth entity not found while trying to log in with OAuth",
      );
    }

    // NOTE: We are calling login hooks here even though we didn't log in the user yet.
    // It's because we have access to the OAuth tokens here and we want to pass them to the hooks.
    // We could have stored the tokens temporarily and called the hooks after the session is created,
    // but this keeps the implementation simpler.
    // The downside of this approach is that we can't provide the session to the login hooks, but this is
    // an okay trade-off because OAuth tokens are more valuable to users than the session ID.
    await hooks.onBeforeLoginHook({
      req,
      providerId,
      user: auth.user,
    });

    // NOTE: check the comment above onBeforeLoginHook for the explanation why we call onAfterLoginHook here.
    await hooks.onAfterLoginHook({
      req,
      providerId,
      oauth,
      user: auth.user,
    });

    return authId;
  } else {
    // The hook runs first so it can veto the signup (by throwing) before the
    // developer's `userSignupFields` getters run.
    await hooks.onBeforeSignupHook({ req, providerId });

    const userFields = await identities.validateAndGetUserFields(
      { profile: providerProfile },
      userSignupFields,
    );

    // For now, we don't have any extra data for the oauth providers, so we just pass an empty object.
    const providerData = await sanitizeAndSerializeProviderData({});

    const user = await identities.createUser(
      providerId,
      providerData,
      // Using any here because we want to avoid TypeScript errors and
      // rely on Prisma to validate the data.
      userFields as any,
    );
    await hooks.onAfterSignupHook({
      req,
      providerId,
      user,
      oauth,
    });

    const auth = identities.getUserAuth(user);
    if (auth === null) {
      throw new Error("Auth entity not found on the user created with OAuth");
    }
    return auth.id;
  }
}
