import { Request, Response } from 'express';
import { verifyPassword } from "../../../core/auth.js";
import { findUserBy, createAuthToken, ensureValidEmailAndPassword } from "../../utils.js";

export function getLoginRoute({
    allowUnverifiedLogin,
}: {
    allowUnverifiedLogin: boolean
}) {
    return async function login(
        req: Request<{ email: string; password: string; }>,
        res: Response
    ): Promise<Response<{ token: string } | undefined>> {
        const args = req.body || {};
        ensureValidEmailAndPassword(args);

        const user = await findUserBy<'email'>({ email: args.email });
        if (!user) {
            return res.status(401).send();
        }
        if (!user.isEmailVerified && !allowUnverifiedLogin) {
            return res.status(401).send();
        }
        try {
            await verifyPassword(user.password, args.password);
        } catch(e) {
            return res.status(401).send();
        }
    
        const token = await createAuthToken(user);
      
        return res.json({ token })
    };
}