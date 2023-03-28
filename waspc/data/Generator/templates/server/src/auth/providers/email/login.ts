import { verifyPassword } from "../../../core/auth.js";
import { handleRejection } from "../../../utils.js";
import { findUserBy, createAuthToken } from "../../utils.js";

export const login = handleRejection(async (req, res) => {
    const args = req.body || {};
    const user = await findUserBy<'email'>({ email: args.email });
    if (!user) {
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