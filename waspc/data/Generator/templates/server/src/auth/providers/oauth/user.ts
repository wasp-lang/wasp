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
import { type OAuthParams } from 'wasp/server/auth'
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
  oauth
}: {
  provider: ProviderConfig;
  providerProfile: unknown;
  providerUserId: string;
  userSignupFields: UserSignupFields | undefined;
  req: ExpressRequest;
  oauth: OAuthParams;
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
  oauth: OAuthParams;
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
    // TODO: it feels weird calling one hook before the other, but we need to call onBeforeLoginHook before onAfterLoginHook
    await onBeforeLoginHook({ req, providerId })

    const authId = existingAuthIdentity.{= authFieldOnAuthIdentityEntityName =}.id

    // Calling findAuthWithUserBy here just to have a user object to pass to the hooks
    const auth = await findAuthWithUserBy({ id: authId })
    
    // TODO: update params, add refresh token
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
