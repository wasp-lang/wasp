import { Request, Response } from 'express';
import {
    createProviderId,
    findAuthIdentity,
    updateAuthIdentityProviderData,
    getProviderDataWithPassword,
} from 'wasp/auth/utils';
import { validateJWT } from 'wasp/auth/jwt'
import { ensureTokenIsPresent, ensurePasswordIsPresent, ensureValidPassword } from 'wasp/auth/validation';
import { HttpError } from 'wasp/server';

export async function resetPassword(
    req: Request<{ token: string; password: string; }>,
    res: Response,
): Promise<void> {
    const args = req.body ?? {};
    ensureValidArgs(args);

    const { token, password } = args;
    const { email } = await validateJWT<{ email: string }>(token)
        .catch(() => {
            throw new HttpError(400, "Password reset failed, invalid token");
        });

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

    res.json({ success: true });
};

function ensureValidArgs(args: object): void {
    ensureTokenIsPresent(args);
    ensurePasswordIsPresent(args);
    ensureValidPassword(args);
}
