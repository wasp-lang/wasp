import { Request, Response } from 'express';
import { waspAuthRuntime } from 'wasp/server/auth/provider';
import { hashPassword } from 'wasp/server/auth/password';
import { validateJWT } from 'wasp/server/auth/jwt'
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

    const emailIdentities = waspAuthRuntime.identityNamespaces('email');
    const identity = await emailIdentities.find(email);
    if (!identity) {
        throw new HttpError(400, "Password reset failed, invalid token");
    }

    // Hashing is the flow's explicit job -- storage never hashes.
    await emailIdentities.setSecrets(email, {
        hashedPassword: await hashPassword(password),
    });
    await emailIdentities.updateData(email, {
        // The act of resetting the password verifies the email
        isEmailVerified: true,
    });

    // Changing the password invalidates all the user's Wasp sessions, so that
    // somebody who got hold of a session can't keep using it -- through the
    // same recursion-safe facet an adapter package gets.
    await waspAuthRuntime.sessions.revokeAllForSubject({
        namespace: 'email',
        subjectId: email,
    });

    res.json({ success: true });
};

function ensureValidPasswordArg(args: object): void {
    ensurePasswordIsPresent(args);
    ensureValidPassword(args);
}
