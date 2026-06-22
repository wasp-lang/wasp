{{={= =}=}}
import { Request as ExpressRequest } from 'express'
import {
  completeOAuthCallback,
  type OAuthProviderName,
} from '@wasp.sh/lib-auth/node'
import {
  createUser,
  validateAndGetUserFields,
  findAuthWithUserBy,
  findAuthIdentity,
  type CreateUserResult,
  type FindAuthWithUserResult,
} from 'wasp/auth/utils'
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
  const { redirectUrl } = await completeOAuthCallback<
    typeof req,
    FindAuthWithUserResult['user'],
    CreateUserResult,
    Record<string, any>,
    OAuthData
  >({
    providerName: provider.id as OAuthProviderName,
    providerUserId,
    providerProfile,
    request: req,
    oauth,
    getUserFields: (providerProfile) =>
      validateAndGetUserFields({ profile: providerProfile }, userSignupFields),
    adapters: {
      authRepository: {
        async findIdentity(providerId) {
          const authIdentity = await findAuthIdentity(providerId)
          return authIdentity === null ? null : {
            authId: authIdentity.authId,
            providerName: providerId.providerName,
            providerUserId: authIdentity.providerUserId,
            providerData: authIdentity.providerData,
          }
        },
        async findAuthWithUserByAuthId(authId) {
          const auth = await findAuthWithUserBy({ id: authId })
          return auth === null ? null : { authId: auth.id, user: auth.user }
        },
        async createUserWithIdentity({
          providerId,
          serializedProviderData,
          userFields,
        }) {
          const user = await createUser(
            providerId,
            serializedProviderData,
            // Using any here because we want to avoid TypeScript errors and
            // rely on Prisma to validate the data.
            userFields as any,
          )
          if (user.auth === null) {
            throw new Error('Auth entity not found after OAuth signup')
          }
          return { authId: user.auth.id, user }
        },
      },
      hooks: {
        onBeforeLogin: ({ request, providerId, user }) =>
          onBeforeLoginHook({ req: request, providerId, user }),
        onAfterLogin: ({ request, providerId, oauth, user }) =>
          onAfterLoginHook({ req: request, providerId, oauth, user }),
        onBeforeSignup: ({ request, providerId }) =>
          onBeforeSignupHook({ req: request, providerId }),
        onAfterSignup: ({ request, providerId, oauth, user }) =>
          onAfterSignupHook({ req: request, providerId, oauth, user }),
      },
      oneTimeCodeStore: {
        createToken: tokenStore.createToken,
      },
      oauthRedirects: {
        getRedirectUrlForOneTimeCode: getRedirectUriForOneTimeCode,
      },
    },
  })

  return redirectUrl
}
