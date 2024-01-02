{{={= =}=}}

import { Router } from "express"
import passport from "passport"

import prisma from '../../../dbClient.js'
import waspServerConfig from '../../../config.js'
import {
  type ProviderName,
  type ProviderId,
  createProviderId,
  authConfig,
  contextWithUserEntity,
  createUser,
  findAuthWithUserBy,
  createAuthToken,
  rethrowPossibleAuthError,
  sanitizeAndSerializeProviderData,
} from "../../utils.js"
import { type {= userEntityUpper =} } from "../../../entities/index.js"
import type { ProviderConfig, RequestWithWasp } from "../types.js"
import type { GetUserFieldsFn } from "./types.js"
import { handleRejection } from "../../../utils.js"

// For oauth providers, we have an endpoint /login to get the auth URL,
// and the /callback endpoint which is used to get the actual access_token and the user info.
export function createRouter(provider: ProviderConfig, initData: { passportStrategyName: string, getUserFieldsFn?: GetUserFieldsFn }) {
    const { passportStrategyName, getUserFieldsFn } = initData;

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
            const userId = await getUserIdFromProviderDetails(providerId, providerProfile, getUserFieldsFn)
            const token = await createAuthToken(userId)
            res.json({ token })
          } catch (e) {
            rethrowPossibleAuthError(e)
          }
      })
    )

    return router;
}

// We need a user id to create the auth token, so we either find an existing user
// or create a new one if none exists for this provider.
async function getUserIdFromProviderDetails(
  providerId: ProviderId,
  providerProfile: any,
  getUserFieldsFn?: GetUserFieldsFn,
): Promise<{= userEntityUpper =}['id']> {
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
    return existingAuthIdentity.{= authFieldOnAuthIdentityEntityName =}.{= userFieldOnAuthEntityName =}.id
  } else {
    const userFields = getUserFieldsFn
      ? await getUserFieldsFn(contextWithUserEntity, { profile: providerProfile })
      : {};

    // For now, we don't have any extra data for the oauth providers, so we just pass an empty object.
    const providerData = await sanitizeAndSerializeProviderData({})
  
    const user = await createUser(
      providerId,
      providerData,
      userFields,
    )

    return user.id
  }
}
