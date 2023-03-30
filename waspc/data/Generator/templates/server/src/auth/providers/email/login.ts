import { Request, Response } from 'express';
import { verifyPassword } from "../../../core/auth.js";
import { handleRejection } from "../../../utils.js";
import { findUserBy, createAuthToken } from "../../utils.js";

export function getLoginRoute({
    allowUnverifiedLogin,
}: {
    allowUnverifiedLogin: boolean
}) {
    return handleRejection(async (req: Request<{ username: string; password: string; }>, res: Response) => {
        const args = req.body || {};
        const user = await findUserBy<'username'>({ username: args.username });
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
    });
}