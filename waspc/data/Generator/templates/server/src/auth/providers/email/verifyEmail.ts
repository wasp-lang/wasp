import { Request, Response } from 'express';
import { validateJWT } from 'wasp/server/auth/jwt';
import {
  createInvalidCredentialsError,
  findAuthWithUserBy,
} from 'wasp/server/auth/utils';
import { getIdentityStore } from 'wasp/server/auth/identityStore';
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

    const emailIdentities = getIdentityStore('email');
    const identity = await emailIdentities.find(email);
    if (!identity) {
        throw new HttpError(400, "Email verification failed, invalid token");
    }

    // A partial update of the non-secret column only -- flipping the flag can
    // no longer race another writer into losing fields, and the password hash
    // is never even read.
    await emailIdentities.updateData(email, {
        isEmailVerified: true,
    });

    const auth = await findAuthWithUserBy({ id: identity.authId })

    if (auth === null) {
        throw createInvalidCredentialsError();
    }

    await onAfterEmailVerifiedHook({ req, email, user: auth.user });

    res.json({ success: true });
};

