import { Request, Response } from 'express';
import {
    createProviderId,
    consumeOneTimeToken,
} from 'wasp/server/auth/utils';
import { validateJWT } from 'wasp/server/auth/jwt'
import { ensureTokenIsPresent, ensurePasswordIsPresent, ensureValidPassword } from 'wasp/auth/validation';
import { HttpError } from 'wasp/server';

export async function resetPassword(
    req: Request<{ token: string; password: string; }>,
    res: Response,
): Promise<void> {
    const args = req.body ?? {};
    ensureValidArgs(args);

    const { token, password } = args;
    const { email, purpose } = await validateJWT<{ email: string; purpose: string }>(token)
        .catch(() => {
            throw new HttpError(400, "Password reset failed, invalid token");
        });

    // Only a token minted for password reset is accepted here.
    if (purpose !== 'reset') {
        throw new HttpError(400, "Password reset failed, invalid token");
    }

    const providerId = createProviderId('email', email);

    // Atomically check + consume the token (one-time use only, even under
    // concurrent requests). Throws if the token is invalid or already used.
    // The new password is hashed when the provider data is persisted.
    await consumeOneTimeToken(
        providerId,
        'outstandingPasswordResetToken',
        token,
        { isEmailVerified: true, hashedPassword: password },
        "Password reset failed, invalid token",
    );

    res.json({ success: true });
};

function ensureValidArgs(args: object): void {
    ensureTokenIsPresent(args);
    ensurePasswordIsPresent(args);
    ensureValidPassword(args);
}
