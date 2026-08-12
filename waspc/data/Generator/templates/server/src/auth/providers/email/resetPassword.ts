import { Request, Response } from 'express';
import {
    createProviderId,
    consumeOneTimeToken,
} from 'wasp/server/auth/utils';
import { validateJWT } from 'wasp/server/auth/jwt'
import { invalidateAllSessionsForAuthId } from 'wasp/server/auth/session'
import { ensureTokenIsPresent, ensurePasswordIsPresent, ensureValidPassword } from 'wasp/auth/validation';
import { HttpError } from 'wasp/server';

export async function resetPassword(
    req: Request<{ token: string; password: string; }>,
    res: Response,
): Promise<void> {
    const args = req.body ?? {};
    // NOTE: The token is validated before the password so that an unauthenticated
    // caller with an invalid token can't learn the deployment's password policy.
    ensureTokenIsPresent(args);

    const { token, password } = args;
    const { email, purpose } = await validateJWT<{ email: string; purpose: string }>(token)
        .catch(() => {
            throw new HttpError(400, "Password reset failed, invalid token");
        });

    // Only a token minted for password reset is accepted here.
    if (purpose !== 'reset') {
        throw new HttpError(400, "Password reset failed, invalid token");
    }

    ensureValidPasswordArg(args);

    const providerId = createProviderId('email', email);

    // Atomically check + consume the token (one-time use only, even under
    // concurrent requests). Throws if the token is invalid or already used.
    // The new password is hashed when the provider data is persisted.
    const authIdentity = await consumeOneTimeToken(
        providerId,
        'outstandingPasswordResetToken',
        token,
        { isEmailVerified: true, hashedPassword: password },
        "Password reset failed, invalid token",
    );

    // Changing the password invalidates all the existing sessions, so that
    // somebody who got hold of a session can't keep using it.
    await invalidateAllSessionsForAuthId(authIdentity.authId);

    res.json({ success: true });
};

function ensureValidPasswordArg(args: object): void {
    ensurePasswordIsPresent(args);
    ensureValidPassword(args);
}
