import { Request, Response } from 'express';
import {
    createProviderId,
    findAuthIdentity,
    updateAuthIdentityProviderData,
    deserializeAndSanitizeProviderData,
} from 'wasp/auth/utils';
import { validateJWT } from 'wasp/auth/jwt'
import { HttpError } from 'wasp/server';


export async function verifyEmail(
    req: Request<{ token: string }>,
    res: Response,
): Promise<Response<{ success: true }>> {
    const { token } = req.body;
    const { email } = await validateJWT<{ email: string }>(token)
        .catch(() => {
            throw new HttpError(400, "Email verification failed, invalid token");
        });

    const providerId = createProviderId('email', email);
    const authIdentity = await findAuthIdentity(providerId);
    if (!authIdentity) {
        throw new HttpError(400, "Email verification failed, invalid token");
    }

    const providerData = deserializeAndSanitizeProviderData<'email'>(authIdentity.providerData);

    await updateAuthIdentityProviderData(providerId, providerData, {
        isEmailVerified: true,
    });

    return res.json({ success: true });
};

