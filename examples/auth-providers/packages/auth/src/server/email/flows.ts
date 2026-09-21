import { hashPassword, verifyPassword } from "@wasp.sh/lib-auth/node";

import {
  HttpError,
  getBody,
  getSignInProperties,
  json,
  sendAuthResponse,
  type Route,
} from "../http.js";
import { offerMergeOrRethrow, requireCurrentAuthId } from "../linking.js";
import type {
  Ctx,
  GetPasswordResetEmailContentFn,
  GetVerificationEmailContentFn,
} from "../types.js";
import {
  createInvalidCredentialsError,
  doFakeWork,
  getHashedPassword,
  makeJwt,
  rethrowPossibleAuthError,
  validateAndGetUserFields,
} from "../utils.js";
import {
  ensurePasswordIsPresent,
  ensureTokenIsPresent,
  ensureValidEmail,
  ensureValidPassword,
  normalizeEmail,
} from "../validation.js";
import { isEmailResendAllowed, makeEmailHelpers } from "./utils.js";

const defaultVerificationEmailContent: GetVerificationEmailContentFn = ({
  verificationLink,
}) => ({
  subject: "Verify your email",
  text: `Click the link below to verify your email: ${verificationLink}`,
  html: `
        <p>Click the link below to verify your email</p>
        <a href="${verificationLink}">Verify email</a>
    `,
});

const defaultPasswordResetEmailContent: GetPasswordResetEmailContentFn = ({
  passwordResetLink,
}) => ({
  subject: "Reset your password",
  text: `Click the link below to reset your password: ${passwordResetLink}`,
  html: `
        <p>Click the link below to reset your password</p>
        <a href="${passwordResetLink}">Reset password</a>
    `,
});

/** The email method: `/auth/email/{signup,login,verify-email,request-password-reset,reset-password}`. */
export function emailRoutes(ctx: Ctx): Route[] {
  const { runtime, spec } = ctx;
  const emailConfig = spec.methods.email!;
  const identities = () => runtime.identities.email;
  const { validateJWT } = makeJwt(runtime);
  const helpers = makeEmailHelpers(runtime);
  const getVerificationEmailContent =
    emailConfig.getVerificationEmailContent ?? defaultVerificationEmailContent;
  const getPasswordResetEmailContent =
    emailConfig.getPasswordResetEmailContent ??
    defaultPasswordResetEmailContent;
  // Wasp allows for auto-verification of emails in development mode to make
  // writing e2e tests easier.
  const isEmailAutoVerified =
    runtime.isDevelopment &&
    runtime.env.SKIP_EMAIL_VERIFICATION_IN_DEV === "true";
  const fromField = emailConfig.fromField;

  async function sendVerificationEmail(email: string): Promise<void> {
    const verificationLink = await helpers.createEmailVerificationLink(
      email,
      emailConfig.emailVerificationClientRoute,
    );
    try {
      await helpers.sendEmailVerificationEmail(email, {
        from: fromField,
        to: email,
        ...getVerificationEmailContent({ verificationLink }),
      });
    } catch (e) {
      console.error("Failed to send email verification email:", e);
      throw new HttpError(500, "Failed to send email verification email.");
    }
  }

  return [
    {
      method: "POST",
      path: "/email/signup",
      handler: async (req, res) => {
        const fields = getBody(req);
        ensureValidEmail(fields);
        ensurePasswordIsPresent(fields);
        ensureValidPassword(fields);
        const email = normalizeEmail(fields.email as string);

        const existingIdentity = await identities().find(email);
        // An already-verified address responds exactly like a fresh signup
        // (no enumeration); an unverified one is superseded after the resend
        // interval, so a squatter cannot lock an address for its real owner.
        if (existingIdentity) {
          if (existingIdentity.data.isEmailVerified) {
            await doFakeWork();
            json(res, 200, { success: true });
            return;
          }
          const { isResendAllowed, timeLeft } = isEmailResendAllowed(
            existingIdentity.data,
            "emailVerificationSentAt",
          );
          if (!isResendAllowed) {
            throw new HttpError(
              400,
              `Please wait ${timeLeft} secs before trying again.`,
            );
          }
          try {
            await identities().deleteUser(email);
          } catch (e) {
            rethrowPossibleAuthError(e);
          }
        }

        try {
          await identities().create(
            email,
            {
              data: {
                isEmailVerified: isEmailAutoVerified ? true : false,
                emailVerificationSentAt: null,
                passwordResetSentAt: null,
              },
              secrets: {
                hashedPassword: await hashPassword(fields.password as string),
              },
            },
            (() =>
              validateAndGetUserFields(
                fields,
                emailConfig.userSignupFields,
              )) as never,
            { req },
          );
        } catch (e) {
          rethrowPossibleAuthError(e);
        }

        if (isEmailAutoVerified) {
          json(res, 200, { success: true });
          return;
        }

        await sendVerificationEmail(email);
        json(res, 200, { success: true });
      },
    },
    {
      // Account linking: an email and password for the signed-in user. The
      // identity starts unverified, so it cannot log in until the emailed
      // link is followed -- the same rule as a signup.
      method: "POST",
      path: "/email/link",
      handler: async (req, res) => {
        const authId = await requireCurrentAuthId(ctx, req);
        const fields = getBody(req);
        ensureValidEmail(fields);
        ensurePasswordIsPresent(fields);
        ensureValidPassword(fields);
        const email = normalizeEmail(fields.email as string);
        try {
          await identities().link(
            email,
            {
              data: {
                isEmailVerified: isEmailAutoVerified ? true : false,
                emailVerificationSentAt: null,
                passwordResetSentAt: null,
              },
              secrets: {
                hashedPassword: await hashPassword(fields.password as string),
              },
            },
            { authId, req },
          );
        } catch (e) {
          // Proof needs the password AND a verified address: an unverified
          // email identity may be a squatter's, not the caller's.
          await offerMergeOrRethrow(ctx, e, {
            intoAuthId: authId,
            findFromAuthId: async () =>
              (await identities().find(email))?.authId ?? null,
            proveControl: async () => {
              const existing = await identities().find(email);
              const hashedPassword = await getHashedPassword(
                identities(),
                email,
              );
              if (
                existing?.data.isEmailVerified !== true ||
                hashedPassword === null
              ) {
                return false;
              }
              return verifyPassword(
                hashedPassword,
                fields.password as string,
              ).then(
                () => true,
                () => false,
              );
            },
          });
        }
        if (!isEmailAutoVerified) {
          await sendVerificationEmail(email);
        }
        json(res, 200, { success: true });
      },
    },
    {
      method: "POST",
      path: "/email/login",
      handler: async (req, res) => {
        const fields = getBody(req);
        ensureValidEmail(fields);
        ensurePasswordIsPresent(fields);
        const email = normalizeEmail(fields.email as string);

        const identity = await identities().find(email);
        if (!identity || !identity.data.isEmailVerified) {
          throw createInvalidCredentialsError();
        }
        const hashedPassword = await getHashedPassword(identities(), email);
        if (hashedPassword === null) {
          throw createInvalidCredentialsError();
        }
        try {
          await verifyPassword(hashedPassword, fields.password as string);
        } catch {
          throw createInvalidCredentialsError();
        }

        const { response } = await runtime.credentialsIssuer.signIn(
          { providerName: "email", providerUserId: email },
          { req, properties: getSignInProperties(fields) },
        );
        sendAuthResponse(res, response);
      },
    },
    {
      method: "POST",
      path: "/email/verify-email",
      handler: async (req, res) => {
        const { token } = getBody(req);
        const { email } = await validateJWT<{ email: string }>(
          token as string,
        ).catch(() => {
          throw new HttpError(400, "Email verification failed, invalid token");
        });
        const identity = await identities().find(email);
        if (!identity) {
          throw new HttpError(400, "Email verification failed, invalid token");
        }
        await identities().updateData(email, { isEmailVerified: true });

        if (spec.onAfterEmailVerified) {
          const auth = await findAuthWithUser(runtime, identity.authId);
          await spec.onAfterEmailVerified({
            prisma: runtime.db,
            req,
            email,
            user: auth?.user,
          });
        }
        json(res, 200, { success: true });
      },
    },
    {
      method: "POST",
      path: "/email/request-password-reset",
      handler: async (req, res) => {
        const args = getBody(req);
        ensureValidEmail(args);
        const email = normalizeEmail(args.email as string);

        const identity = await identities().find(email);
        // Fake work: an unknown address takes as long as a known one.
        if (!identity) {
          await doFakeWork();
          json(res, 200, { success: true });
          return;
        }
        const { isResendAllowed, timeLeft } = isEmailResendAllowed(
          identity.data,
          "passwordResetSentAt",
        );
        if (!isResendAllowed) {
          throw new HttpError(
            400,
            `Please wait ${timeLeft} secs before trying again.`,
          );
        }
        const passwordResetLink = await helpers.createPasswordResetLink(
          email,
          emailConfig.passwordResetClientRoute,
        );
        try {
          await helpers.sendPasswordResetEmail(email, {
            from: fromField,
            to: email,
            ...getPasswordResetEmailContent({ passwordResetLink }),
          });
        } catch (e) {
          console.error("Failed to send password reset email:", e);
          throw new HttpError(500, "Failed to send password reset email.");
        }
        json(res, 200, { success: true });
      },
    },
    {
      method: "POST",
      path: "/email/reset-password",
      handler: async (req, res) => {
        const args = getBody(req);
        // The token is validated before the password so that an
        // unauthenticated caller with an invalid token can't learn the
        // deployment's password policy.
        ensureTokenIsPresent(args);
        const { email } = await validateJWT<{ email: string }>(
          args.token as string,
        ).catch(() => {
          throw new HttpError(400, "Password reset failed, invalid token");
        });
        ensurePasswordIsPresent(args);
        ensureValidPassword(args);

        const identity = await identities().find(email);
        if (!identity) {
          throw new HttpError(400, "Password reset failed, invalid token");
        }
        await identities().updateSecrets(email, {
          hashedPassword: await hashPassword(args.password as string),
        });
        // The act of resetting the password verifies the email.
        await identities().updateData(email, { isEmailVerified: true });
        // Changing the password invalidates every existing credential, so
        // that somebody who got hold of one can't keep using it.
        await runtime.credentialsIssuer.signOutEverywhere({
          providerName: "email",
          providerUserId: email,
        });
        json(res, 200, { success: true });
      },
    },
  ];
}

/**
 * The business user behind an auth id, for the method-specific hooks' `user`
 * parameter. Through the app's PrismaClient: the `Auth` model and its `user`
 * relation are fixed by Wasp's schema injection, whatever the app calls its
 * user entity.
 */
export async function findAuthWithUser(
  runtime: Ctx["runtime"],
  authId: string,
): Promise<{ id: string; user: unknown } | null> {
  const db = runtime.db as {
    auth: {
      findUnique(args: unknown): Promise<{ id: string; user: unknown } | null>;
    };
  };
  const auth = await db.auth.findUnique({
    where: { id: authId },
    include: { user: true },
  });
  return auth !== null && auth.user !== null ? auth : null;
}
