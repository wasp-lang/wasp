import { Request, Response } from 'express';
import { validateJWT } from 'wasp/auth/jwt';
import {
  createProviderId,
  findAuthIdentity,
  findAuthWithUserBy,
  getProviderDataWithPassword,
  updateAuthIdentityProviderData,
} from 'wasp/auth/utils';
import { HttpError } from 'wasp/server';
import { onAfterEmailVerifiedHook } from '../../hooks.js';


export async function verifyEmail(
    req: Request<{ token: string }>,
    res: Response,
): Promise<void> {
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

    const providerData = getProviderDataWithPassword<'email'>(authIdentity.providerData);

    await updateAuthIdentityProviderData(providerId, providerData, {
        isEmailVerified: true,
    });

    const auth = await findAuthWithUserBy({ id: authIdentity.authId })

    await onAfterEmailVerifiedHook({ req, email, user: auth.user });

    res.json({ success: true });
};

