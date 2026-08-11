import { Request, Response } from 'express';
import { validateJWT } from 'wasp/server/auth/jwt';
import {
  createProviderId,
  findAuthIdentity,
  findAuthWithUserBy,
  getProviderDataWithPassword,
  sha256,
  updateAuthIdentityProviderData,
} from 'wasp/server/auth/utils';
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

    // The token must match the currently outstanding (unused) email
    // verification token. This makes each verification URL one-time use only.
    if (providerData.outstandingEmailVerificationToken !== sha256(token)) {
        throw new HttpError(400, "Email verification failed, invalid token");
    }

    await updateAuthIdentityProviderData(providerId, providerData, {
        isEmailVerified: true,
        // Consume the token so the same URL can't be used again.
        outstandingEmailVerificationToken: null,
    });

    const auth = await findAuthWithUserBy({ id: authIdentity.authId })

    await onAfterEmailVerifiedHook({ req, email, user: auth.user });

    res.json({ success: true });
};

