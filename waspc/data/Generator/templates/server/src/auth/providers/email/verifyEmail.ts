import { Request, Response } from 'express';
import { validateJWT } from 'wasp/server/auth/jwt';
import {
  createInvalidCredentialsError,
  createProviderId,
  findAuthIdentity,
  findAuthWithUserBy,
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

    // A partial update of the non-secret column only -- flipping the flag can
    // no longer race another writer into losing fields, and the password hash
    // is never even read.
    await updateAuthIdentityProviderData<'email'>(providerId, {
        isEmailVerified: true,
    });

    const auth = await findAuthWithUserBy({ id: authIdentity.authId })

    if (auth === null) {
        throw createInvalidCredentialsError();
    }

    await onAfterEmailVerifiedHook({ req, email, user: auth.user });

    res.json({ success: true });
};

