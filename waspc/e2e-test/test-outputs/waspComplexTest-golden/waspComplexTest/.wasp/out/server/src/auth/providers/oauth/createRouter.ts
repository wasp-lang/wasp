
import { Router } from "express"
import passport from "passport"

import prisma from '../../../dbClient.js'
import waspServerConfig from '../../../config.js'
import { sign } from '../../../core/auth.js'
import {
  authConfig,
  contextWithUserEntity,
  createAuthWithUser,
  findAuthWithUserBy,
} from "../../utils.js"

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

          // Wrap call to getUserFieldsFn so we can invoke only if needed.
          const getUserFields = () => getUserFieldsFn ? getUserFieldsFn(contextWithUserEntity, { profile: providerProfile }) : Promise.resolve({});
          // TODO: In the future we could make this configurable, possibly associating an external account
          // with the currently logged in account, or by some DB lookup.
          const auth = await findOrCreateAuthByAuthIdentity(provider.id, providerProfile.id, getUserFields);

          const token = await sign(auth.user.id);
          res.json({ token });
      })
    )

    return router;
}

async function findOrCreateAuthByAuthIdentity(
  providerName: string,
  providerUserId: string,
  getUserFields: () => ReturnType<GetUserFieldsFn>,
) {
  // Attempt to find a User by an external auth association.
  const authIdentity = await prisma.authIdentity.findFirst({
    where: { providerName, providerUserId },
    include: {
      auth: {
        include: {
          user: true
        }
      }
    }
  })

  if (authIdentity) {
    return authIdentity.auth
  }

  const userFields = await getUserFields()

  const auth = await createAuthWithUser(providerName, providerUserId, undefined, userFields)
  // NOTE: we are fetching the auth again becuase it incldues nested user
  return findAuthWithUserBy({ id: auth.id });
}
