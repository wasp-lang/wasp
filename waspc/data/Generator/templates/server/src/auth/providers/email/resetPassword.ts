import { handleRejection } from "../../../utils.js";
import { findUserBy, updateUserPassword, verifyToken } from "../../utils.js";

export const resetPassword = handleRejection(async (req, res) => {
    const args = req.body || {};
    const { token, newPassword } = args;
    try {
        const { id: userId } = await verifyToken(token);
        const user = await findUserBy<'id'>({ id: userId });
        if (!user) {
            return res.status(400).json({ error: 'Invalid token' });
        }
        await updateUserPassword(userId, newPassword);
    } catch (e) {
        res.status(400).json({ error: 'Invalid token' });
        return;
    }
    res.json({ success: true });
});