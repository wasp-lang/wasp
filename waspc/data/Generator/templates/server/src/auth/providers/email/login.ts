import { Request, Response } from 'express';
import { throwInvalidCredentialsError } from 'wasp/auth/utils'
import { verifyPassword } from 'wasp/auth/password'
import {
    createProviderId,
    findAuthIdentity,
    findAuthWithUserBy,
    deserializeAndSanitizeProviderData,
} from 'wasp/auth/utils'
import { createSession } from 'wasp/auth/session'
import { ensureValidEmail, ensurePasswordIsPresent } from 'wasp/auth/validation'
import { onBeforeLoginHook, onAfterLoginHook } from '../../hooks.js';

export function getLoginRoute() {
    return async function login(
        req: Request<{ email: string; password: string; }>,
        res: Response,
    ): Promise<Response<{ sessionId: string } | undefined>> {
        const fields = req.body ?? {}
        ensureValidArgs(fields)

        const providerId = createProviderId("email", fields.email)
        const authIdentity = await findAuthIdentity(providerId)
        if (!authIdentity) {
            throwInvalidCredentialsError()
        }
        const providerData = deserializeAndSanitizeProviderData<'email'>(authIdentity.providerData)
        if (!providerData.isEmailVerified) {
            throwInvalidCredentialsError()
        }
        try {
            await verifyPassword(providerData.hashedPassword, fields.password);
        } catch(e) {
            throwInvalidCredentialsError()
        }
    
        const auth = await findAuthWithUserBy({ id: authIdentity.authId })
        
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
      
        return res.json({
            sessionId: session.id,
        })
    };
}

function ensureValidArgs(args: unknown): void {
    ensureValidEmail(args);
    ensurePasswordIsPresent(args);
}
