{{={= =}=}}

import { Router } from "express"
import passport from "passport"

import prisma from '../../../dbClient.js'
import waspServerConfig from '../../../config.js'
import { sign } from '../../../core/auth.js'
import { authConfig, contextWithUserEntity } from "../../utils.js"

import type { {= userEntityUpper =} } from '../../../entities';
import type { ProviderConfig, RequestWithWasp } from "../types.js"
import type { GetUserFn } from "./types.js"

// For oauth providers, we have an endpoint /login to get the auth URL,
// and the /callback endpoint which is used to get the actual access_token and the user info.
export function createRouter(provider: ProviderConfig, initData: { passportStrategyName: string, getUserFn: GetUserFn }) {
    const { passportStrategyName, getUserFn } = initData;

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
        async function (req: RequestWithWasp, res) {
            const providerProfile = req?.wasp?.providerProfile;

            if (!providerProfile) {
                throw new Error(`Missing ${provider.displayName} provider profile on request. This should not happen! Please contact Wasp.`);
            } else if (!providerProfile.id) {
                throw new Error(`${provider.displayName} provider profile was missing required id property. This should not happen! Please contact Wasp.`);
            }

            const user = await prisma.$transaction(async (tx) => {
              // Wrap call to getUserFn so we can invoke only if needed.
              const getUser = () => getUserFn({ entities: { {= userEntityUpper =}: tx.{= userEntityLower =} } }, { profile: providerProfile });
              return await findOrCreateUserByExternalAuthAssociation(provider.id, providerProfile.id, getUser, tx);
            })

            const token = await sign(user.id);
            res.json({ token });
        }
    )

    return router;
}

async function findOrCreateUserByExternalAuthAssociation(
  provider: string,
  providerId: string,
  getUser: () => ReturnType<GetUserFn>,
  tx: any,
): Promise<{= userEntityUpper =}> {
  // Attempt to find a User by an external auth association.
  const externalAuthAssociation = await prisma.{= externalAuthEntityLower =}.findFirst({
    where: { provider, providerId },
    include: { user: true }
  })

  if (externalAuthAssociation) {
    return externalAuthAssociation.user
  }

  // No external auth association linkage found. Get a user by invoking `getUser()`.
  // Additionally, associate the externalAuthAssociations with the new User.
  const user = await getUser()

  if (!user?.id) {
    throw new Error(`The getUser() function must return a {= userEntityUpper =} object with an id property.`);
  }

  return tx.{= userEntityLower =}.update({
    where: {
      id: user.id,
    },
    data: {
      externalAuthAssociations: {
        create: [{ provider, providerId }]
      }
    }
  })
}
