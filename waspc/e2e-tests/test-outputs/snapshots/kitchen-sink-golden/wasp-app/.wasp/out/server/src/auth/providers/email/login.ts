import { Request, Response } from 'express';
import { createInvalidCredentialsError } from 'wasp/server/auth/utils'
import { verifyPassword } from 'wasp/server/auth/password'
import {
    createProviderId,
    findAuthWithUserBy,
} from 'wasp/server/auth/utils'
import { getIdentityStore } from 'wasp/server/auth/identityStore'
import { createSession } from 'wasp/server/auth/session'
import { ensureValidEmail, ensurePasswordIsPresent } from 'wasp/auth/validation'
import { onBeforeLoginHook, onAfterLoginHook } from '../../hooks.js';

export function getLoginRoute() {
    return async function login(
        req: Request<{ email: string; password: string; }>,
        res: Response,
    ): Promise<void> {
        const fields = req.body ?? {}
        ensureValidArgs(fields)

        const emailIdentities = getIdentityStore('email')
        const providerId = createProviderId("email", fields.email)
        const identity = await emailIdentities.find(fields.email)
        if (!identity) {
            throw createInvalidCredentialsError()
        }
        if (!identity.data.isEmailVerified) {
            throw createInvalidCredentialsError()
        }
        // The secret column is read explicitly and only here in this flow.
        const secrets = await emailIdentities.getSecrets(fields.email)
        if (secrets === null) {
            throw createInvalidCredentialsError()
        }
        try {
            await verifyPassword(secrets.hashedPassword, fields.password);
        } catch(e) {
            throw createInvalidCredentialsError()
        }
    
        const auth = await findAuthWithUserBy({ id: identity.authId })

        if (auth === null) {
            throw createInvalidCredentialsError()
        }
        
        await onBeforeLoginHook({
            req,
            providerId,
            user: auth.user,
        })
        
        const session = await createSession(auth.id)

        await onAfterLoginHook({
            req,
            providerId,
            user: auth.user,
        })
      
        res.json({
            sessionId: session.id,
        })
    };
}

function ensureValidArgs(args: object): void {
    ensureValidEmail(args);
    ensurePasswordIsPresent(args);
}
