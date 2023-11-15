import { Request, Response } from 'express';
import { verifyPassword, throwInvalidCredentialsError } from "../../../core/auth.js";
import { findAuthWithUserBy, createAuthToken } from "../../utils.js";
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
        const userFields = req.body || {}
        ensureValidArgs(userFields)

        userFields.email = userFields.email.toLowerCase()

        const auth = await findAuthWithUserBy({ email: userFields.email })
        if (!auth) {
            throwInvalidCredentialsError()
        }
        if (!auth.isEmailVerified && !allowUnverifiedLogin) {
            throwInvalidCredentialsError()
        }
        try {
            await verifyPassword(auth.password, userFields.password);
        } catch(e) {
            throwInvalidCredentialsError()
        }
    
        const token = await createAuthToken(auth)
      
        return res.json({ token })
    };
}

function ensureValidArgs(args: unknown): void {
    ensureValidEmail(args);
    ensurePasswordIsPresent(args);
}
