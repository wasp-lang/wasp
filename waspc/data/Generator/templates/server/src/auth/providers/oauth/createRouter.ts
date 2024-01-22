{{={= =}=}}

import { Router } from "express"
import passport from "passport"

import prisma from 'wasp/server/dbClient'
import waspServerConfig from 'wasp/server/config'
import {
  type ProviderName,
  type ProviderId,
  createProviderId,
  authConfig,
  contextWithUserEntity,
  createUser,
  rethrowPossibleAuthError,
  sanitizeAndSerializeProviderData,
  validateAndGetUserFields,
} from 'wasp/auth/utils'
import { createSession } from "wasp/auth/session"
import { type {= authEntityUpper =} } from "wasp/entities"
import type { ProviderConfig, RequestWithWasp, UserSignupFields } from "wasp/auth/providers/types"
import { handleRejection } from "wasp/server/utils"

// For oauth providers, we have an endpoint /login to get the auth URL,
// and the /callback endpoint which is used to get the actual access_token and the user info.
export function createRouter(provider: ProviderConfig, initData: {
  passportStrategyName: string,
  userSignupFields?: UserSignupFields,
}) {
    const { passportStrategyName, userSignupFields } = initData;

    const router = Router();

    // Constructs a provider OAuth URL and redirects browser to start sign in flow.
    router.get('/login', passport.authenticate(passportStrategyName, { session: false }));

    // Validates the OAuth code from the frontend, via server-to-server communication
    // with provider. If valid, provides frontend a response containing the JWT.
    // NOTE: `addProviderProfileToRequest` is invoked as part of the `passport.authenticate`
    // call, before the final route handler callback. This is how we gain access to `req.wasp.providerProfile`.
    router.get('/callback',
        passport.authenticate(passportStrategyName, {
            session: false,
            failureRedirect: waspServerConfig.frontendUrl + authConfig.failureRedirectPath
        }),
        handleRejection(async function (req: RequestWithWasp, res) {
          const providerProfile = req?.wasp?.providerProfile;

          if (!providerProfile) {
              throw new Error(`Missing ${provider.displayName} provider profile on request. This should not happen! Please contact Wasp.`);
          } else if (!providerProfile.id) {
              throw new Error(`${provider.displayName} provider profile was missing required id property. This should not happen! Please contact Wasp.`);
          }

          const providerId = createProviderId(provider.id, providerProfile.id);

          try {
            const authId = await getAuthIdFromProviderDetails(providerId, providerProfile, userSignupFields)
            const session = await createSession(authId)
            return res.json({
              sessionId: session.id,
            })
          } catch (e) {
            rethrowPossibleAuthError(e)
          }
      })
    )

    return router;
}

// We need a user id to create the auth token, so we either find an existing user
// or create a new one if none exists for this provider.
async function getAuthIdFromProviderDetails(
  providerId: ProviderId,
  providerProfile: any,
  userSignupFields?: UserSignupFields,
): Promise<{= authEntityUpper =}['id']> {
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
    return existingAuthIdentity.{= authFieldOnAuthIdentityEntityName =}.id
  } else {
    const userFields = await validateAndGetUserFields(
      { profile: providerProfile },
      userSignupFields,
    );

    // For now, we don't have any extra data for the oauth providers, so we just pass an empty object.
    const providerData = await sanitizeAndSerializeProviderData({})
  
    const user = await createUser(
      providerId,
      providerData,
      // Using any here because we want to avoid TypeScript errors and
      // rely on Prisma to validate the data.
      userFields as any,
    )

    return user.auth.id
  }
}
