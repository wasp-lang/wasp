import { Request, Response } from 'express';
import { loginWithEmail } from '@wasp.sh/lib-auth/node'
import {
    findAuthIdentity,
    findAuthWithUserBy,
    rethrowPossibleAuthServiceError,
} from 'wasp/auth/utils'
import { createSession } from 'wasp/auth/session'
import { onBeforeLoginHook, onAfterLoginHook } from '../../hooks.js';

export function getLoginRoute() {
    return async function login(
        req: Request<{ email: string; password: string; }>,
        res: Response,
    ): Promise<void> {
        const fields = req.body ?? {}

        try {
            const { sessionId } = await loginWithEmail({
                fields,
                request: req,
                adapters: {
                    authRepository: {
                        async findIdentity(providerId) {
                            const authIdentity = await findAuthIdentity(providerId)
                            return authIdentity === null ? null : {
                                authId: authIdentity.authId,
                                providerName: 'email',
                                providerUserId: authIdentity.providerUserId,
                                providerData: authIdentity.providerData,
                            }
                        },
                        async findAuthWithUserByAuthId(authId) {
                            const auth = await findAuthWithUserBy({ id: authId })
                            return auth === null ? null : { authId: auth.id, user: auth.user }
                        },
                    },
                    sessionService: {
                        createSession,
                    },
                    hooks: {
                        onBeforeLogin: ({ request, providerId, user }) =>
                            onBeforeLoginHook({ req: request, providerId, user }),
                        onAfterLogin: ({ request, providerId, user }) =>
                            onAfterLoginHook({ req: request, providerId, user }),
                    },
                },
            })

            res.json({
                sessionId,
            })
        } catch (e: unknown) {
            rethrowPossibleAuthServiceError(e)
            throw e
        }
    };
}
