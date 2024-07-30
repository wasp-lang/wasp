{{={= =}=}}
import { Request as ExpressRequest } from 'express'
import {
  type ProviderId,
  createUser,
  sanitizeAndSerializeProviderData,
  validateAndGetUserFields,
  createProviderId,
  findAuthWithUserBy,
} from 'wasp/auth/utils'
import { type {= authEntityUpper =} } from 'wasp/entities'
import { prisma } from 'wasp/server'
import { type UserSignupFields, type ProviderConfig } from 'wasp/auth/providers/types'
import { getRedirectUriForOneTimeCode } from './redirect'
import { tokenStore } from './oneTimeCode'
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
  accessToken,
  oAuthState,
}: {
  provider: ProviderConfig;
  providerProfile: unknown;
  providerUserId: string;
  userSignupFields: UserSignupFields | undefined;
  req: ExpressRequest;
  accessToken: string;
  oAuthState: { state: string };
}): Promise<URL> {
  const providerId = createProviderId(provider.id, providerUserId);

  const authId = await getAuthIdFromProviderDetails({
    providerId,
    providerProfile,
    userSignupFields,
    req,
    accessToken,
    oAuthState,
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
  accessToken,
  oAuthState,
}: {
  providerId: ProviderId;
  providerProfile: any;
  userSignupFields: UserSignupFields | undefined;
  req: ExpressRequest;
  accessToken: string;
  oAuthState: { state: string };
}): Promise<{= authEntityUpper =}['id']> {
  const existingAuthIdentity = await prisma.{= authIdentityEntityLower =}.findUnique({
    where: {
      providerName_providerUserId: providerId,
    },
    include: {
      {= authFieldOnAuthIdentityEntityName =}: {
        include: {
          {= userFieldOnAuthEntityName =}: true
        }
      }
    }
  })

  if (existingAuthIdentity) {
    const authId = existingAuthIdentity.{= authFieldOnAuthIdentityEntityName =}.id

    // NOTE: We are calling login hooks here even though we didn't log in the user yet.
    // We are doing it here because we have access to the OAuth tokens and we can pass them to the hooks.
    // This isn't a big deal because the next step of the OAuth flow happens immediately after this function
    // and the user is redirected to the client with the one-time code which is then used to create the session.
    // The downside of this approach is that we can't provide the session to the login hooks, but this is
    // an okay trade-off for now.
    await onBeforeLoginHook({ req, providerId })

    // NOTE: Fetching the user to pass it to the onAfterLoginHook - it's a bit wasteful
    // but we wanted to keep the onAfterLoginHook params consistent for all auth providers.
    const auth = await findAuthWithUserBy({ id: authId })

    await onAfterLoginHook({
      req,
      providerId,
      oauth: {
        accessToken,
        uniqueRequestId: oAuthState.state,
      },
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
      oauth: {
        accessToken,
        uniqueRequestId: oAuthState.state,
      },
    })

    return user.auth.id
  }
}
