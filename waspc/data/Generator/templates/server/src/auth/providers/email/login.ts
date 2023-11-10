import { Request, Response } from 'express';
import { verifyPassword, throwInvalidCredentialsError } from "../../../core/auth.js";
import { findUserBy, createAuthToken } from "../../utils.js";
import { ensureValidEmailAndPassword } from "../../validation.js";

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
        ensureValidEmailAndPassword(userFields)

        userFields.email = userFields.email.toLowerCase()

        const user = await findUserBy({ email: userFields.email })
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
