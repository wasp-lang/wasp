import { Request, Response } from 'express';
import { validateJWT } from 'wasp/server/auth/jwt';
import {
  createInvalidCredentialsError,
  createProviderId,
  consumeOneTimeToken,
  findAuthWithUserBy,
} from 'wasp/server/auth/utils';
import { HttpError } from 'wasp/server';
import { onAfterEmailVerifiedHook } from '../../hooks.js';


export async function verifyEmail(
    req: Request<{ token: string }>,
    res: Response,
): Promise<void> {
    const { token } = req.body;
    const { email, purpose } = await validateJWT<{ email: string; purpose: string }>(token)
        .catch(() => {
            throw new HttpError(400, "Email verification failed, invalid token");
        });

    // Only a token minted for email verification is accepted here.
    if (purpose !== 'verify') {
        throw new HttpError(400, "Email verification failed, invalid token");
    }

    const providerId = createProviderId('email', email);

    // Atomically check + consume the token (one-time use only, even under
    // concurrent requests). Throws if the token is invalid or already used.
    const authIdentity = await consumeOneTimeToken(
        providerId,
        'outstandingEmailVerificationToken',
        token,
        { isEmailVerified: true },
        "Email verification failed, invalid token",
    );

    const auth = await findAuthWithUserBy({ id: authIdentity.authId })

    if (auth === null) {
        throw createInvalidCredentialsError();
    }

    await onAfterEmailVerifiedHook({ req, email, user: auth.user });

    res.json({ success: true });
};

