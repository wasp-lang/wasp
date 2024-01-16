import { Request, Response } from 'express';
import { throwInvalidCredentialsError } from '../../utils.js'
import { verifyPassword } from '../../password.js'
import {
    createProviderId,
    findAuthIdentity,
    findAuthWithUserBy,
    deserializeAndSanitizeProviderData,
} from '../../utils.js'
import { createSession } from '../../session.js'
import { ensureValidEmail, ensurePasswordIsPresent } from '../../validation.js'

export function getLoginRoute() {
    return async function login(
        req: Request<{ email: string; password: string; }>,
        res: Response,
    ): Promise<Response<{ sessionId: string } | undefined>> {
        const fields = req.body ?? {}
        ensureValidArgs(fields)

        const authIdentity = await findAuthIdentity(
            createProviderId("email", fields.email)
        )
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
        const session = await createSession(auth.id)
      
        return res.json({
            sessionId: session.id,
        })
    };
}

function ensureValidArgs(args: unknown): void {
    ensureValidEmail(args);
    ensurePasswordIsPresent(args);
}
