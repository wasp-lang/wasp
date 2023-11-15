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

        const user = await findAuthWithUserBy({ email: userFields.email })
        if (!user) {
            throwInvalidCredentialsError()
        }
        if (!user.isEmailVerified && !allowUnverifiedLogin) {
            throwInvalidCredentialsError()
        }
        try {
            await verifyPassword(user.password, userFields.password);
        } catch(e) {
            throwInvalidCredentialsError()
        }
    
        const token = await createAuthToken(user)
      
        return res.json({ token })
    };
}

function ensureValidArgs(args: unknown): void {
    ensureValidEmail(args);
    ensurePasswordIsPresent(args);
}
