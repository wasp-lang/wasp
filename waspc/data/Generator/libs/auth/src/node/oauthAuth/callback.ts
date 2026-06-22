import type { ProviderIdFor } from "../authService/types";
import {
  createProviderId,
  sanitizeAndSerializeProviderData,
  type PossibleProviderData,
} from "../providerData";
import type {
  OAuthCallbackArgs,
  OAuthCallbackErrorRedirectArgs,
  OAuthCallbackResult,
  OAuthHookData,
  OAuthProviderName,
} from "./types";

export async function completeOAuthCallback<
  RequestContext,
  User,
  CreatedUser,
  UserFields extends object = object,
  OAuthData extends OAuthHookData = OAuthHookData,
>({
  providerName,
  providerUserId,
  providerProfile,
  request,
  oauth,
  getUserFields,
  adapters,
}: OAuthCallbackArgs<
  RequestContext,
  User,
  CreatedUser,
  UserFields,
  OAuthData
>): Promise<OAuthCallbackResult> {
  const providerId = createProviderId(
    providerName,
    providerUserId,
  ) as ProviderIdFor<OAuthProviderName>;
  const authId = await getAuthIdFromProviderDetails({
    providerId,
    providerProfile,
    request,
    oauth,
    getUserFields,
    adapters,
  });
  const oneTimeCode = await adapters.oneTimeCodeStore.createToken(authId);

  return {
    redirectUrl:
      adapters.oauthRedirects.getRedirectUrlForOneTimeCode(oneTimeCode),
  };
}

export function getOAuthCallbackErrorRedirectUrl({
  error,
  adapters,
}: OAuthCallbackErrorRedirectArgs): OAuthCallbackResult {
  return {
    redirectUrl: adapters.oauthRedirects.getFailureRedirectUrl(error),
  };
}

async function getAuthIdFromProviderDetails<
  RequestContext,
  User,
  CreatedUser,
  UserFields extends object,
  OAuthData extends OAuthHookData,
>({
  providerId,
  providerProfile,
  request,
  oauth,
  getUserFields,
  adapters,
}: Pick<
  OAuthCallbackArgs<RequestContext, User, CreatedUser, UserFields, OAuthData>,
  "providerProfile" | "request" | "oauth" | "getUserFields" | "adapters"
> & {
  providerId: ProviderIdFor<OAuthProviderName>;
}): Promise<string> {
  const existingAuthIdentity =
    await adapters.authRepository.findIdentity(providerId);

  if (existingAuthIdentity) {
    const auth = await adapters.authRepository.findAuthWithUserByAuthId(
      existingAuthIdentity.authId,
    );

    if (auth === null) {
      throw new Error(
        "Auth entity not found while trying to log in with OAuth",
      );
    }

    await adapters.hooks.onBeforeLogin({
      request,
      providerId,
      user: auth.user,
    });
    await adapters.hooks.onAfterLogin({
      request,
      providerId,
      oauth,
      user: auth.user,
    });

    return auth.authId;
  }

  const userFields = await getUserFields?.(providerProfile);
  const serializedProviderData = await sanitizeAndSerializeProviderData(
    {} as PossibleProviderData[OAuthProviderName],
  );

  await adapters.hooks.onBeforeSignup({ request, providerId });
  const createdUser = await adapters.authRepository.createUserWithIdentity({
    providerId,
    serializedProviderData,
    userFields,
  });
  await adapters.hooks.onAfterSignup({
    request,
    providerId,
    user: createdUser.user,
    oauth,
  });

  return createdUser.authId;
}
