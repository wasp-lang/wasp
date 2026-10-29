import { Request, Response } from 'express';
import {
    createProviderId,
    findAuthIdentity,
    updateAuthIdentityProviderData,
    getProviderDataWithPassword,
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
    const { email } = await validateJWT<{ email: string }>(token)
        .catch(() => {
            throw new HttpError(400, "Password reset failed, invalid token");
        });

    ensureValidPasswordArg(args);

    const providerId = createProviderId('email', email);
    const authIdentity = await findAuthIdentity(providerId);
    if (!authIdentity) {
        throw new HttpError(400, "Password reset failed, invalid token");
    }

    const providerData = getProviderDataWithPassword<'email'>(authIdentity.providerData);

    await updateAuthIdentityProviderData(providerId, providerData, {
        // The act of resetting the password verifies the email
        isEmailVerified: true,
        // The password will be hashed when saving the providerData
        // in the DB
        hashedPassword: password,
    });

    // Changing the password invalidates all the existing sessions, so that
    // somebody who got hold of a session can't keep using it.
    await invalidateAllSessionsForAuthId(authIdentity.authId);

    res.json({ success: true });
};

function ensureValidPasswordArg(args: object): void {
    ensurePasswordIsPresent(args);
    ensureValidPassword(args);
}
