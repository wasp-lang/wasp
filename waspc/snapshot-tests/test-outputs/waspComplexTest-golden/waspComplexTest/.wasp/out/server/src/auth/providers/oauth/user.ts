import { Request as ExpressRequest } from 'express'
import {
  type ProviderId,
  createUser,
  sanitizeAndSerializeProviderData,
  validateAndGetUserFields,
  createProviderId,
  findAuthWithUserBy,
} from 'wasp/auth/utils'
import { type Auth } from 'wasp/entities'
import { prisma } from 'wasp/server'
import { type UserSignupFields, type ProviderConfig } from 'wasp/auth/providers/types'
import { type OAuthData } from 'wasp/server/auth'
import { getRedirectUriForOneTimeCode, tokenStore } from 'wasp/server/auth'
import {
  onBeforeSignupHook,
  onAfterSignupHook,
  onBeforeLoginHook,
  onAfterLoginHook,
} from '../../hooks.js'

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

  const authId = await getAuthIdFromProviderDetails({
    providerId,
    providerProfile,
    userSignupFields,
    req,
    oauth,
  });

  const oneTimeCode = await tokenStore.createToken(authId)

  return getRedirectUriForOneTimeCode(oneTimeCode)
}

// We need a user id to create the auth token, so we either find an existing user
// or create a new one if none exists for this provider.
async function getAuthIdFromProviderDetails({
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
}): Promise<Auth['id']> {
  const existingAuthIdentity = await prisma.authIdentity.findUnique({
    where: {
      providerName_providerUserId: providerId,
    },
    include: {
      auth: {
        include: {
          user: true
        }
      }
    }
  })

  if (existingAuthIdentity) {
    const authId = existingAuthIdentity.auth.id

    // NOTE: Fetching the user to pass it to the login hooks - it's a bit wasteful
    // but we wanted to keep the onAfterLoginHook params consistent for all auth providers.
    const auth = await findAuthWithUserBy({ id: authId })

    if (auth === null) {
        throw new Error('Auth entity not found while trying to log in with OAuth')
    }

    // NOTE: We are calling login hooks here even though we didn't log in the user yet.
    // It's because we have access to the OAuth tokens here and we want to pass them to the hooks.
    // We could have stored the tokens temporarily and called the hooks after the session is created,
    // but this keeps the implementation simpler.
    // The downside of this approach is that we can't provide the session to the login hooks, but this is
    // an okay trade-off because OAuth tokens are more valuable to users than the session ID.
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

    return authId
  } else {
    const userFields = await validateAndGetUserFields(
      { profile: providerProfile },
      userSignupFields,
    )

    // For now, we don't have any extra data for the oauth providers, so we just pass an empty object.
    const providerData = await sanitizeAndSerializeProviderData({})
  
    await onBeforeSignupHook({ req, providerId })
    const user = await createUser(
      providerId,
      providerData,
      // Using any here because we want to avoid TypeScript errors and
      // rely on Prisma to validate the data.
      userFields as any,
    )
    await onAfterSignupHook({
      req,
      providerId,
      user,
      oauth,
    })

    return user.auth.id
  }
}
