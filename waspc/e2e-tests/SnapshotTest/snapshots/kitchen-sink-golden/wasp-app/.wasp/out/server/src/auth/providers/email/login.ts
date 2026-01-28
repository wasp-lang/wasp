import { Request, Response } from 'express';
import { createInvalidCredentialsError } from 'wasp/auth/utils'
import { verifyPassword } from 'wasp/auth/password'
import {
    createProviderId,
    findAuthIdentity,
    findAuthWithUserBy,
    getProviderDataWithPassword,
} from 'wasp/auth/utils'
import { createSession } from 'wasp/auth/session'
import { ensureValidEmail, ensurePasswordIsPresent } from 'wasp/auth/validation'
import { onBeforeLoginHook, onAfterLoginHook } from '../../hooks.js';

export function getLoginRoute() {
    return async function login(
        req: Request<{ email: string; password: string; }>,
        res: Response,
    ): Promise<void> {
        const fields = req.body ?? {}
        ensureValidArgs(fields)

        const providerId = createProviderId("email", fields.email)
        const authIdentity = await findAuthIdentity(providerId)
        if (!authIdentity) {
            throw createInvalidCredentialsError()
        }
        const providerData = getProviderDataWithPassword<'email'>(authIdentity.providerData)
        if (!providerData.isEmailVerified) {
            throw createInvalidCredentialsError()
        }
        try {
            await verifyPassword(providerData.hashedPassword, fields.password);
        } catch(e) {
            throw createInvalidCredentialsError()
        }
    
        const auth = await findAuthWithUserBy({ id: authIdentity.authId })

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
