import { Request, Response } from 'express';
import { createInvalidCredentialsError } from 'wasp/server/auth/utils'
import { verifyPassword } from 'wasp/server/auth/password'
import { waspAuthRuntime } from 'wasp/server/auth/provider'
import { ensureValidEmail, ensurePasswordIsPresent } from 'wasp/auth/validation'

export function getLoginRoute() {
    return async function login(
        req: Request<{ email: string; password: string; }>,
        res: Response,
    ): Promise<void> {
        const fields = req.body ?? {}
        ensureValidArgs(fields)

        const emailIdentities = waspAuthRuntime.identityNamespaces('email')
        const identity = await emailIdentities.find(fields.email)
        if (!identity) {
            throw createInvalidCredentialsError()
        }
        if (!identity.data.isEmailVerified) {
            throw createInvalidCredentialsError()
        }
        // The secret column is read explicitly and only here in this flow.
        const secrets = await emailIdentities.getSecrets(fields.email)
        if (secrets === null || typeof secrets.hashedPassword !== 'string') {
            throw createInvalidCredentialsError()
        }
        try {
            await verifyPassword(secrets.hashedPassword, fields.password);
        } catch(e) {
            throw createInvalidCredentialsError()
        }

        // The mint goes through the same `wasp-sessions` facet an adapter
        // package gets; the app's login hooks fire inside it.
        const { sessionId } = await waspAuthRuntime.sessions.issue(
            { namespace: 'email', subjectId: fields.email },
            { req },
        )

        res.json({
            sessionId,
        })
    };
}

function ensureValidArgs(args: object): void {
    ensureValidEmail(args);
    ensurePasswordIsPresent(args);
}
