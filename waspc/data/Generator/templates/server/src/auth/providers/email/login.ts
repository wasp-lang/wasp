import { Request, Response } from 'express';
import { verifyPassword, throwInvalidCredentialsError } from "../../../core/auth.js";
import {
    findAuthIdentity,
    findAuthWithUserBy,
    createAuthToken,
    deserializeAndSanitizeProviderData,
} from "../../utils.js";
import { ensureValidEmail, ensurePasswordIsPresent } from "../../validation.js";

export function getLoginRoute({
    allowUnverifiedLogin,
}: {
    allowUnverifiedLogin: boolean
}) {
    return async function login(
        req: Request<{ email: string; password: string; }>,
        res: Response,
    ): Promise<Response<{ token: string } | undefined>> {
        const fields = req.body ?? {}
        ensureValidArgs(fields)

        const authIdentity = await findAuthIdentity("email", fields.email)
        if (!authIdentity) {
            throwInvalidCredentialsError()
        }
        const providerData = deserializeAndSanitizeProviderData<'email'>(authIdentity.providerData)
        if (!providerData.isEmailVerified && !allowUnverifiedLogin) {
            throwInvalidCredentialsError()
        }
        try {
            await verifyPassword(providerData.hashedPassword, fields.password);
        } catch(e) {
            throwInvalidCredentialsError()
        }
    
        const auth = await findAuthWithUserBy({ id: authIdentity.authId })
        const token = await createAuthToken(auth.userId)
      
        return res.json({ token })
    };
}

function ensureValidArgs(args: unknown): void {
    ensureValidEmail(args);
    ensurePasswordIsPresent(args);
}
