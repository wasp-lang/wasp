import { Request as ExpressRequest } from 'express'
import {
  type ProviderId,
  validateAndGetUserFields,
  createProviderId,
  findAuthWithUserBy,
} from 'wasp/server/auth/utils'
import { waspAuthRuntime } from 'wasp/server/auth/provider'
import { type UserSignupFields, type ProviderConfig } from 'wasp/auth/providers/types'
import { type OAuthData } from 'wasp/server/auth'
import { getRedirectUriForOneTimeCode, tokenStore } from 'wasp/server/auth'
import {
  onBeforeLoginHook,
  onAfterLoginHook,
} from 'wasp/server/auth/hookDispatch'

export async function finishOAuthFlowAndGetRedirectUri({
  provider,
  providerProfile,
  providerUserId,
  userSignupFields,
  req,
  oauth
}: {
  provider: ProviderConfig;
  providerProfile: unknown;
  providerUserId: string;
  userSignupFields: UserSignupFields | undefined;
  req: ExpressRequest;
  oauth: OAuthData;
}): Promise<URL> {
  const providerId = createProviderId(provider.id, providerUserId);

  await ensureSubjectExistsAndRunHooks({
    providerId,
    providerProfile,
    userSignupFields,
    req,
    oauth,
  });

  const oneTimeCode = await tokenStore.createToken({
    namespace: providerId.providerName,
    subjectId: providerId.providerUserId,
  })

  return getRedirectUriForOneTimeCode(oneTimeCode)
}

// We either find an existing subject or create a new one if none exists for
// this provider, firing the app's hooks HERE (not at the later code
// redemption): this is the moment the OAuth tokens exist, and the hooks
// receive them.
async function ensureSubjectExistsAndRunHooks({
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
  oauth: OAuthData;
}): Promise<void> {
  const identities = waspAuthRuntime.identityNamespaces(providerId.providerName)
  const existingIdentity = await identities.find(providerId.providerUserId)

  if (existingIdentity) {
    // NOTE: Fetching the user to pass it to the login hooks - it's a bit wasteful
    // but we wanted to keep the onAfterLoginHook params consistent for all auth providers.
    const auth = await findAuthWithUserBy({ id: existingIdentity.authId })

    if (auth === null) {
        throw new Error('Auth entity not found while trying to log in with OAuth')
    }

    // NOTE: We are calling login hooks here even though we didn't log in the user yet.
    // It's because we have access to the OAuth tokens here and we want to pass them to the hooks.
    // The later one-time-code redemption mints with `skipHooks`, so the hooks
    // fire exactly once per login -- here, where the tokens are.
    await onBeforeLoginHook({
      req,
      providerId,
      user: auth.user,
    })

    // NOTE: check the comment above onBeforeLoginHook for the explanation why we call onAfterLoginHook here.
    await onAfterLoginHook({
      req,
      providerId,
      oauth,
      user: auth.user,
    })
  } else {
    // The identity facet's `create` is the signup choke point: the app's
    // onBeforeSignup veto fires first, then the lazy `userSignupFields`
    // getters, then the atomic write, then onAfterSignup (with the tokens
    // as its `oauth` payload).
    // For now, we don't store any data or secrets for the oauth providers.
    await identities.create(
      providerId.providerUserId,
      {},
      // Using any because we want to rely on Prisma to validate the data.
      (() => validateAndGetUserFields(
        { profile: providerProfile },
        userSignupFields,
      )) as any,
      { req, hookContext: oauth },
    )
  }
}
