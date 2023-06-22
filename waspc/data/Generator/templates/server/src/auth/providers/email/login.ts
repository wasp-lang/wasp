import { Request, Response } from 'express';
import { verifyPassword, throwInvalidCredentialsError } from "../../../core/auth.js";
import { findUserBy, createAuthToken, ensureValidEmailAndPassword } from "../../utils.js";

export function getLoginRoute({
    allowUnverifiedLogin,
}: {
    allowUnverifiedLogin: boolean
}) {
    return async function login(
        req: Request<{ email: string; password: string; }>,
        res: Response,
    ): Promise<Response<{ token: string } | undefined>> {
        const args = req.body || {}
        ensureValidEmailAndPassword(args)

        args.email = args.email.toLowerCase()

        const user = await findUserBy<'email'>({ email: args.email })
        if (!user) {
            throwInvalidCredentialsError()
        }
        if (!user.isEmailVerified && !allowUnverifiedLogin) {
            throwInvalidCredentialsError()
        }
        try {
            await verifyPassword(user.password, args.password);
        } catch(e) {
            throwInvalidCredentialsError()
        }
    
        const token = await createAuthToken(user)
      
        return res.json({ token })
    };
}
