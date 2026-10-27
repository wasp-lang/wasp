/**
 * Server-side auth APIs: types for your
 * [auth hooks](https://wasp.sh/docs/features/auth/hooks), defining extra
 * signup fields, helpers for working with users' auth identities (e.g. in
 * [custom sign-up actions](https://wasp.sh/docs/features/auth/advanced/custom-auth-actions)),
 * and the OAuth clients of your social auth providers.
 *
 * Available when your app enables
 * [auth](https://wasp.sh/docs/features/auth/overview). Some exports depend on
 * the auth methods your app enables:
 *
 * - With [Email](https://wasp.sh/docs/features/auth/email/overview) auth:
 *   {@link ensureValidEmail}, {@link createEmailVerificationLink},
 *   {@link sendEmailVerificationEmail}, {@link createPasswordResetLink},
 *   {@link sendPasswordResetEmail}, {@link isEmailResendAllowed},
 *   {@link GetVerificationEmailContentFn}, and
 *   {@link GetPasswordResetEmailContentFn}.
 * - With
 *   [Username & password](https://wasp.sh/docs/features/auth/username-and-pass/overview)
 *   auth: {@link ensureValidUsername}.
 * - With a [social auth](https://wasp.sh/docs/features/auth/social-auth/overview)
 *   provider: that provider's object with its OAuth client, e.g.
 *   {@link google} for Google.
 *
 * @module
 */

import type { PossibleProviderData, ProviderName } from "@wasp.sh/lib-sdk-core";
import type { EmailFromField } from "@wasp.sh/lib-sdk-core/node";
import type { Request } from "express";
import type { PrismaClient } from "../server";

// Entities

/**
 * Your app's user entity: the Prisma model you set as `auth.userEntity` in
 * your app spec (usually `User`).
 *
 * Wasp generates the real type from your Prisma schema, so it has the fields
 * you defined on that model.
 */
interface UserEntity {}

/**
 * Wasp's `Auth` entity, which connects a user to their auth identities.
 *
 * @see [The `Auth` entity](https://wasp.sh/docs/features/auth/entities#entities-explained)
 */
interface AuthEntity {
  /** The `Auth` entity's ID. */
  id: string;
}

/**
 * Wasp's `AuthIdentity` entity, which stores a user's data for one auth
 * method.
 *
 * @see [The `AuthIdentity` entity](https://wasp.sh/docs/features/auth/entities#entities-explained)
 */
interface AuthIdentityEntity {
  /** The auth method's name, e.g. `"email"` or `"google"`. */
  providerName: string;
  /** The user's ID for the auth method, e.g. their email or Google ID. */
  providerUserId: string;
  /**
   * The auth method's data for the user, serialized as JSON. Use
   * {@link getProviderData} to read it.
   */
  providerData: string;
  /** The ID of the `Auth` entity this identity belongs to. */
  authId: string;
}

// Signup fields

/**
 * A function that computes the value of an extra signup field.
 *
 * @param data The data the client sent in the signup request, without the
 * `password` field.
 * @returns The value to save in the user entity field. Throw an error if the
 * data is invalid.
 */
type UserSignupFieldFn = (data: { [key: string]: unknown }) => unknown;

/**
 * An object that tells Wasp how to compute your extra signup fields. Its keys
 * are fields of your user entity, and its values are
 * {@link UserSignupFieldFn | functions} that compute the field's value from
 * the data sent from the client.
 */
type UserSignupFields = { [field: string]: UserSignupFieldFn };

/**
 * Defines the extra fields Wasp saves on your user entity when a user signs
 * up, with the correct types.
 *
 * Export its result and pass it as the `userSignupFields` option of an auth
 * method in your app spec (e.g. `auth.methods.usernameAndPassword.userSignupFields`).
 *
 * The object's keys are the field names, and they need to exist on your user
 * entity. The values are functions that receive the data sent from the
 * client and return the value of the field. Wasp excludes the `password`
 * field from that data, so it isn't saved as plain text in the database;
 * Wasp's auth backend handles the password.
 *
 * If the value the function received is invalid, the function should throw
 * an error. Wasp then rejects the signup with that error message.
 *
 * You can use any validation library (e.g. `zod`) in the functions.
 *
 * For social auth providers, the data the functions receive also includes
 * the user's profile from the provider; see each provider's docs.
 *
 * @param fields The extra signup fields.
 * @returns The same `fields` object, typed.
 *
 * @example
 * ```ts
 * // main.wasp.ts
 * import { app } from "@wasp.sh/spec";
 * import { userSignupFields } from "./src/auth/signup" with { type: "ref" };
 *
 * export default app({
 *   // ...
 *   auth: {
 *     userEntity: "User",
 *     methods: {
 *       usernameAndPassword: {
 *         userSignupFields,
 *       },
 *     },
 *     onAuthFailedRedirectTo: "/login",
 *   },
 * });
 * ```
 *
 * ```ts
 * // src/auth/signup.ts
 * import { defineUserSignupFields } from "wasp/server/auth";
 *
 * export const userSignupFields = defineUserSignupFields({
 *   address: async (data) => {
 *     const address = data.address;
 *     if (typeof address !== "string") {
 *       throw new Error("Address is required");
 *     }
 *     if (address.length < 5) {
 *       throw new Error("Address must be at least 5 characters long");
 *     }
 *     return address;
 *   },
 * });
 * ```
 *
 * @example
 * Validating with `zod`:
 *
 * ```ts
 * import { defineUserSignupFields } from "wasp/server/auth";
 * import * as z from "zod";
 *
 * export const userSignupFields = defineUserSignupFields({
 *   address: (data) => {
 *     const AddressSchema = z
 *       .string({
 *         required_error: "Address is required",
 *         invalid_type_error: "Address must be a string",
 *       })
 *       .min(10, "Address must be at least 10 characters long");
 *     const result = AddressSchema.safeParse(data.address);
 *     if (result.success === false) {
 *       throw new Error(result.error.issues[0].message);
 *     }
 *     return result.data;
 *   },
 * });
 * ```
 *
 * @see [Customizing the signup process](https://wasp.sh/docs/features/auth/overview#customizing-the-signup-process)
 */
export declare function defineUserSignupFields<T extends UserSignupFields>(
  fields: T,
): T;

// Auth identities

/**
 * Uniquely identifies a user's auth identity: the auth method and the user's
 * ID for it. For example, the `"email"` provider with the user ID
 * `"test@test.com"`, or the `"google"` provider with the user ID
 * `"1234567890"`.
 *
 * Wasp passes it to the `onBeforeSignup`, `onAfterSignup`, `onBeforeLogin`,
 * and `onAfterLogin` hooks. Create one with {@link createProviderId}.
 */
export type ProviderId = {
  /** The auth method's name (e.g. `"email"`, `"google"`, `"github"`). */
  providerName: ProviderName;
  /**
   * The user's unique ID in the provider's system (e.g. their email, Google
   * ID, or GitHub ID).
   */
  providerUserId: string;
};

/**
 * The name of an auth method, as used in {@link ProviderId}.
 */
export type { ProviderName } from "@wasp.sh/lib-sdk-core";

/**
 * The data Wasp stores for a user's
 * [Email](https://wasp.sh/docs/features/auth/email/overview) auth identity.
 *
 * @see [Accessing user data](https://wasp.sh/docs/features/auth/entities)
 */
export type { EmailProviderData } from "@wasp.sh/lib-sdk-core";

/**
 * The data Wasp stores for a user's
 * [Username & password](https://wasp.sh/docs/features/auth/username-and-pass/overview)
 * auth identity.
 *
 * @see [Accessing user data](https://wasp.sh/docs/features/auth/entities)
 */
export type { UsernameProviderData } from "@wasp.sh/lib-sdk-core";

/**
 * The data Wasp stores for a user's
 * [social auth](https://wasp.sh/docs/features/auth/social-auth/overview)
 * identity. It's empty.
 */
export type { OAuthProviderData } from "@wasp.sh/lib-sdk-core";

/**
 * Creates a {@link ProviderId} that identifies a user's auth identity.
 *
 * It normalizes the user ID: emails and usernames are case-insensitive, so it
 * lowercases them.
 *
 * @param providerName The auth method's name, e.g. `"email"`.
 * @param providerUserId The user's ID for the auth method, e.g. their email.
 *
 * @example
 * ```ts
 * import { createProviderId, findAuthIdentity } from "wasp/server/auth";
 *
 * const providerId = createProviderId("email", args.email);
 * const authIdentity = await findAuthIdentity(providerId);
 * ```
 *
 * @see [Custom signup action](https://wasp.sh/docs/features/auth/entities#custom-signup-action)
 */
export declare function createProviderId(
  providerName: ProviderName,
  providerUserId: string,
): ProviderId;

/**
 * Finds the auth identity of a user.
 *
 * @param providerId The auth identity to find. Create it with
 * {@link createProviderId}.
 * @returns The `AuthIdentity` entity, or `null` if it doesn't exist.
 *
 * @example
 * ```ts
 * import { createProviderId, findAuthIdentity } from "wasp/server/auth";
 *
 * const providerId = createProviderId("email", args.email);
 * const authIdentity = await findAuthIdentity(providerId);
 * if (!authIdentity) {
 *   throw new HttpError(400, "Unknown user");
 * }
 * ```
 */
export declare function findAuthIdentity(
  providerId: ProviderId,
): Promise<AuthIdentityEntity | null>;

/**
 * Reads the auth method's data from an `AuthIdentity` entity's
 * `providerData` field, without the hashed password.
 *
 * Pass the auth method's name as the type parameter to get the right type,
 * e.g. `getProviderData<"email">(...)`.
 *
 * @param providerData The `providerData` field of an `AuthIdentity` entity.
 *
 * @example
 * ```ts
 * import { getProviderData } from "wasp/server/auth";
 *
 * const providerData = getProviderData<"email">(authIdentity.providerData);
 * if (providerData.isEmailVerified) {
 *   // ...
 * }
 * ```
 */
export { getProviderData } from "@wasp.sh/lib-sdk-core";

/**
 * Reads the auth method's data from an `AuthIdentity` entity's
 * `providerData` field, including the hashed password.
 *
 * Pass the auth method's name as the type parameter to get the right type,
 * e.g. `getProviderDataWithPassword<"email">(...)`.
 *
 * @param providerData The `providerData` field of an `AuthIdentity` entity.
 *
 * @example
 * ```ts
 * import { getProviderDataWithPassword } from "wasp/server/auth";
 *
 * const providerData = getProviderDataWithPassword<"email">(
 *   authIdentity.providerData,
 * );
 * ```
 *
 * @see [Password hashing](https://wasp.sh/docs/features/auth/overview#password-hashing)
 */
export { getProviderDataWithPassword } from "@wasp.sh/lib-sdk-core";

/**
 * Prepares an auth method's data for saving in an `AuthIdentity` entity's
 * `providerData` field: hashes the password (if the data has a
 * `hashedPassword` field) and serializes the data as JSON.
 *
 * Pass the auth method's name as the type parameter to get the right type,
 * e.g. `sanitizeAndSerializeProviderData<"username">(...)`.
 *
 * @param providerData The auth method's data, with the plain-text password in
 * the `hashedPassword` field.
 * @returns The serialized data, to pass to {@link createUser}.
 *
 * @example
 * ```ts
 * import {
 *   createProviderId,
 *   createUser,
 *   sanitizeAndSerializeProviderData,
 * } from "wasp/server/auth";
 *
 * const providerId = createProviderId("username", args.username);
 * // Hashes the password and returns a JSON string.
 * const providerData = await sanitizeAndSerializeProviderData<"username">({
 *   hashedPassword: args.password,
 * });
 * await createUser(providerId, providerData, {});
 * ```
 *
 * @see [Custom signup action](https://wasp.sh/docs/features/auth/entities#custom-signup-action)
 */
export declare function sanitizeAndSerializeProviderData<
  PN extends ProviderName,
>(providerData: PossibleProviderData[PN]): Promise<string>;

/**
 * Updates the auth method's data of a user's auth identity.
 *
 * It hashes the password if you update the `hashedPassword` field, so pass
 * the new password **not hashed**.
 *
 * @param providerId The auth identity to update. Create it with
 * {@link createProviderId}.
 * @param existingProviderData The identity's current data, e.g. from
 * {@link getProviderDataWithPassword}.
 * @param providerDataUpdates The fields to update.
 * @returns The updated `AuthIdentity` entity.
 *
 * @example
 * ```ts
 * import {
 *   createProviderId,
 *   findAuthIdentity,
 *   updateAuthIdentityProviderData,
 *   getProviderDataWithPassword,
 * } from "wasp/server/auth";
 * import type { UpdatePassword } from "wasp/server/operations";
 *
 * export const updatePassword: UpdatePassword<
 *   { email: string; password: string },
 *   void
 * > = async (args, context) => {
 *   const providerId = createProviderId("email", args.email);
 *   const authIdentity = await findAuthIdentity(providerId);
 *   if (!authIdentity) {
 *     throw new HttpError(400, "Unknown user");
 *   }
 *
 *   const providerData = getProviderDataWithPassword<"email">(
 *     authIdentity.providerData,
 *   );
 *
 *   // Updates the password and hashes it automatically.
 *   await updateAuthIdentityProviderData(providerId, providerData, {
 *     hashedPassword: args.password,
 *   });
 * };
 * ```
 *
 * @see [Password hashing](https://wasp.sh/docs/features/auth/overview#password-hashing)
 */
export declare function updateAuthIdentityProviderData<PN extends ProviderName>(
  providerId: ProviderId,
  existingProviderData: PossibleProviderData[PN],
  providerDataUpdates: Partial<PossibleProviderData[PN]>,
): Promise<AuthIdentityEntity>;

/**
 * The user that {@link createUser} creates: your user entity, with its `auth`
 * relation.
 */
export type CreateUserResult = UserEntity & {
  /** The user's `Auth` entity. */
  auth: AuthEntity | null;
};

/**
 * Creates a user with an auth identity.
 *
 * It's a more convenient way to create the `User`, `Auth`, and
 * `AuthIdentity` entities than using Prisma's `User.create` directly.
 *
 * @param providerId The user's auth identity. Create it with
 * {@link createProviderId}.
 * @param serializedProviderData The auth method's data for the user. Create
 * it with {@link sanitizeAndSerializeProviderData}.
 * @param userFields Any additional data you want to store on the user
 * entity.
 * @returns The created user, with its `auth` relation.
 *
 * @example
 * ```ts
 * import {
 *   createProviderId,
 *   sanitizeAndSerializeProviderData,
 *   createUser,
 * } from "wasp/server/auth";
 *
 * const providerId = createProviderId("username", args.username);
 * const providerData = await sanitizeAndSerializeProviderData<"username">({
 *   hashedPassword: args.password,
 * });
 *
 * await createUser(
 *   providerId,
 *   providerData,
 *   // Any additional data you want to store on the User entity
 *   {},
 * );
 *
 * // This is equivalent to:
 * // await User.create({
 * //   data: {
 * //     auth: {
 * //       create: {
 * //         identities: {
 * //           create: {
 * //             providerName: "username",
 * //             providerUserId: args.username,
 * //             providerData,
 * //           },
 * //         },
 * //       },
 * //     },
 * //   },
 * // });
 * ```
 *
 * @see [Custom signup action](https://wasp.sh/docs/features/auth/entities#custom-signup-action)
 */
export declare function createUser(
  providerId: ProviderId,
  serializedProviderData?: string,
  userFields?: { [field: string]: unknown },
): Promise<CreateUserResult>;

// Validators

/**
 * Checks that `args.password` is present, and throws an `HttpError` with
 * status 422 if it isn't.
 *
 * It's one of the validators Wasp uses in its default auth flows, which you
 * can use in your
 * [custom sign-up actions](https://wasp.sh/docs/features/auth/advanced/custom-auth-actions).
 *
 * @param args The data sent from the client.
 *
 * @example
 * ```ts
 * import {
 *   ensurePasswordIsPresent,
 *   ensureValidPassword,
 *   ensureValidUsername,
 * } from "wasp/server/auth";
 *
 * export const customSignup: CustomSignup<
 *   CustomSignupInput,
 *   CustomSignupOutput
 * > = async (args, _context) => {
 *   ensureValidUsername(args);
 *   ensurePasswordIsPresent(args);
 *   ensureValidPassword(args);
 *   // ...
 * };
 * ```
 */
export { ensurePasswordIsPresent } from "@wasp.sh/lib-sdk-core";

/**
 * Checks that `args.password` is a valid password, and throws an `HttpError`
 * with status 422 if it isn't.
 *
 * A valid password has at least 8 characters and contains a number.
 *
 * It's one of the validators Wasp uses in its default auth flows, which you
 * can use in your
 * [custom sign-up actions](https://wasp.sh/docs/features/auth/advanced/custom-auth-actions).
 *
 * @param args The data sent from the client.
 *
 * @see [Default validations](https://wasp.sh/docs/features/auth/overview#default-validations)
 */
export { ensureValidPassword } from "@wasp.sh/lib-sdk-core";

/**
 * Checks that `args.token` is present, and throws an `HttpError` with status
 * 422 if it isn't.
 *
 * @param args The data sent from the client.
 */
export { ensureTokenIsPresent } from "@wasp.sh/lib-sdk-core";

/**
 * Checks that `args.email` is present and a valid email address, and throws
 * an `HttpError` with status 422 if it isn't.
 *
 * Wasp's definition of a valid email address is the HTML5 one, widened to
 * support Unicode.
 *
 * Only available with
 * [Email](https://wasp.sh/docs/features/auth/email/overview) auth. It's one of
 * the validators Wasp uses in its default auth flows, which you can use in
 * your
 * [custom sign-up actions](https://wasp.sh/docs/features/auth/advanced/custom-auth-actions).
 *
 * @param args The data sent from the client.
 *
 * @see [Default validations](https://wasp.sh/docs/features/auth/overview#default-validations)
 */
export { ensureValidEmail } from "@wasp.sh/lib-sdk-core";

/**
 * Checks that `args.username` is present, and throws an `HttpError` with
 * status 422 if it isn't.
 *
 * Only available with
 * [Username & password](https://wasp.sh/docs/features/auth/username-and-pass/overview)
 * auth. It's one of the validators Wasp uses in its default auth flows, which
 * you can use in your
 * [custom sign-up actions](https://wasp.sh/docs/features/auth/advanced/custom-auth-actions).
 *
 * @param args The data sent from the client.
 *
 * @see [Default validations](https://wasp.sh/docs/features/auth/overview#default-validations)
 */
export { ensureValidUsername } from "@wasp.sh/lib-sdk-core";

// Email auth

/**
 * The content of an email.
 */
interface Email {
  /** The sender. Defaults to the `defaultFrom` of your email sender. */
  from?: EmailFromField;
  /** The recipient's email address. */
  to: string;
  /** The email's subject. */
  subject: string;
  /** The email's plain-text content. */
  text: string;
  /** The email's HTML content. */
  html: string;
}

/**
 * Creates a link that verifies the user's email. The link points to the
 * `clientRoute` of your app, with a `token` query parameter that expires in
 * 30 minutes.
 *
 * Only available with
 * [Email](https://wasp.sh/docs/features/auth/email/overview) auth.
 *
 * @param email The user's email.
 * @param clientRoute The path of the client route that verifies the email
 * (e.g. `"/email-verification"`), where you use
 * [`VerifyEmailForm`](https://wasp.sh/docs/api/wasp/client/auth/functions/VerifyEmailForm)
 * or call
 * [`verifyEmail`](https://wasp.sh/docs/api/wasp/client/auth/functions/verifyEmail).
 * @returns The full verification link.
 *
 * @example
 * ```ts
 * import {
 *   createEmailVerificationLink,
 *   sendEmailVerificationEmail,
 * } from "wasp/server/auth";
 *
 * // Verification link links to a client route e.g. /email-verification
 * const verificationLink = await createEmailVerificationLink(
 *   args.email,
 *   "/email-verification",
 * );
 * await sendEmailVerificationEmail(args.email, {
 *   from: {
 *     name: "My App Postman",
 *     email: "hello@itsme.com",
 *   },
 *   to: args.email,
 *   subject: "Verify your email",
 *   text: `Click the link below to verify your email: ${verificationLink}`,
 *   html: `
 *     <p>Click the link below to verify your email</p>
 *     <a href="${verificationLink}">Verify email</a>
 *   `,
 * });
 * ```
 *
 * @see [Custom sign-up actions](https://wasp.sh/docs/features/auth/advanced/custom-auth-actions)
 */
export declare function createEmailVerificationLink(
  email: string,
  clientRoute: string,
): Promise<string>;

/**
 * Sends the email verification email to a user, and saves when it was sent
 * (in the `emailVerificationSentAt` field of their {@link EmailProviderData}).
 *
 * It sends the email in the background: it doesn't wait for it to be sent,
 * and it logs any errors sending it.
 *
 * Only available with
 * [Email](https://wasp.sh/docs/features/auth/email/overview) auth.
 *
 * @param email The email of the user. It throws an error if there's no user
 * with that email.
 * @param content The email to send. Create its verification link with
 * {@link createEmailVerificationLink}.
 *
 * @see [Custom sign-up actions](https://wasp.sh/docs/features/auth/advanced/custom-auth-actions)
 */
export declare function sendEmailVerificationEmail(
  email: string,
  content: Email,
): Promise<void>;

/**
 * Creates a link that lets the user reset their password. The link points to
 * the `clientRoute` of your app, with a `token` query parameter that expires
 * in 30 minutes.
 *
 * Only available with
 * [Email](https://wasp.sh/docs/features/auth/email/overview) auth.
 *
 * @param email The user's email.
 * @param clientRoute The path of the client route that resets the password
 * (e.g. `"/password-reset"`), where you use
 * [`ResetPasswordForm`](https://wasp.sh/docs/api/wasp/client/auth/functions/ResetPasswordForm)
 * or call
 * [`resetPassword`](https://wasp.sh/docs/api/wasp/client/auth/functions/resetPassword).
 * @returns The full password reset link.
 */
export declare function createPasswordResetLink(
  email: string,
  clientRoute: string,
): Promise<string>;

/**
 * Sends the password reset email to a user, and saves when it was sent (in
 * the `passwordResetSentAt` field of their {@link EmailProviderData}).
 *
 * It sends the email in the background: it doesn't wait for it to be sent,
 * and it logs any errors sending it.
 *
 * Only available with
 * [Email](https://wasp.sh/docs/features/auth/email/overview) auth.
 *
 * @param email The email of the user. It throws an error if there's no user
 * with that email.
 * @param content The email to send. Create its password reset link with
 * {@link createPasswordResetLink}.
 */
export declare function sendPasswordResetEmail(
  email: string,
  content: Email,
): Promise<void>;

/**
 * Checks if enough time has passed since Wasp sent the user an email, so you
 * can send it again. Use it to rate-limit sending emails.
 *
 * Only available with
 * [Email](https://wasp.sh/docs/features/auth/email/overview) auth.
 *
 * @param fields The user's {@link EmailProviderData}.
 * @param field The field with the time the email was sent:
 * `"emailVerificationSentAt"` or `"passwordResetSentAt"`.
 * @param resendInterval The minimum time between emails, in milliseconds.
 * Defaults to 1 minute.
 * @returns Whether you can resend the email (`isResendAllowed`), and if not,
 * how many seconds are left until you can (`timeLeft`).
 *
 * @example
 * ```ts
 * import { getProviderData, isEmailResendAllowed } from "wasp/server/auth";
 *
 * const providerData = getProviderData<"email">(authIdentity.providerData);
 * const { isResendAllowed, timeLeft } = isEmailResendAllowed(
 *   providerData,
 *   "passwordResetSentAt",
 * );
 * if (!isResendAllowed) {
 *   throw new HttpError(400, `Please wait ${timeLeft} secs before trying again.`);
 * }
 * ```
 */
export declare function isEmailResendAllowed<
  Field extends "emailVerificationSentAt" | "passwordResetSentAt",
>(
  fields: { [field in Field]: string | null },
  field: Field,
  resendInterval?: number,
): { isResendAllowed: boolean; timeLeft: number };

/**
 * The type of the function that creates the content of the email
 * verification email, which you can set as
 * `auth.methods.email.emailVerification.getEmailContentFn`.
 *
 * Only available with
 * [Email](https://wasp.sh/docs/features/auth/email/overview) auth.
 *
 * @example
 * ```ts
 * import type { GetVerificationEmailContentFn } from "wasp/server/auth";
 *
 * export const getVerificationEmailContent: GetVerificationEmailContentFn = ({
 *   verificationLink,
 * }) => ({
 *   subject: "Verify your email",
 *   text: `Click the link below to verify your email: ${verificationLink}`,
 *   html: `
 *     <p>Click the link below to verify your email</p>
 *     <a href="${verificationLink}">Verify email</a>
 *   `,
 * });
 * ```
 *
 * @see [`EmailFlowConfig`](https://wasp.sh/docs/api/@wasp.sh/spec/interfaces/EmailFlowConfig)
 */
export type { GetVerificationEmailContentFn } from "@wasp.sh/lib-sdk-core/node";

/**
 * The type of the function that creates the content of the password reset
 * email, which you can set as
 * `auth.methods.email.passwordReset.getEmailContentFn`.
 *
 * Only available with
 * [Email](https://wasp.sh/docs/features/auth/email/overview) auth.
 *
 * @example
 * ```ts
 * import type { GetPasswordResetEmailContentFn } from "wasp/server/auth";
 *
 * export const getPasswordResetEmailContent: GetPasswordResetEmailContentFn = ({
 *   passwordResetLink,
 * }) => ({
 *   subject: "Password reset",
 *   text: `Click the link below to reset your password: ${passwordResetLink}`,
 *   html: `
 *     <p>Click the link below to reset your password</p>
 *     <a href="${passwordResetLink}">Reset password</a>
 *   `,
 * });
 * ```
 *
 * @see [`EmailFlowConfig`](https://wasp.sh/docs/api/@wasp.sh/spec/interfaces/EmailFlowConfig)
 */
export type { GetPasswordResetEmailContentFn } from "@wasp.sh/lib-sdk-core/node";

// Hooks

/**
 * The input all auth hooks receive.
 */
interface CommonAuthHookParams {
  /**
   * The Prisma client instance, which you can use to query your database.
   * It's the same as `prisma` from `wasp/server`.
   */
  prisma: PrismaClient;
  /**
   * The [Express request object](https://expressjs.com/en/api.html#req) of
   * the request that triggered the hook, from which you can access the
   * request headers, cookies, etc.
   */
  req: Request;
}

/** The input of the {@link OnBeforeSignupHook | `onBeforeSignup`} hook. */
interface OnBeforeSignupHookParams extends CommonAuthHookParams {
  /** The auth identity the user is signing up with. */
  providerId: ProviderId;
}

/** The input of the {@link OnAfterSignupHook | `onAfterSignup`} hook. */
interface OnAfterSignupHookParams extends CommonAuthHookParams {
  /** The auth identity the user signed up with. */
  providerId: ProviderId;
  /** The user that was created. */
  user: CreateUserResult;
  /**
   * The OAuth flow's data. Only present if the user signed up with
   * [social auth](https://wasp.sh/docs/features/auth/social-auth/overview).
   */
  oauth?: OAuthData;
}

/**
 * The input of the {@link OnAfterEmailVerifiedHook | `onAfterEmailVerified`}
 * hook.
 */
interface OnAfterEmailVerifiedHookParams extends CommonAuthHookParams {
  /** The user's verified email address. */
  email: string;
  /** The user who completed email verification. */
  user: UserEntity;
}

/**
 * The input of the {@link OnBeforeOAuthRedirectHook | `onBeforeOAuthRedirect`}
 * hook.
 */
interface OnBeforeOAuthRedirectHookParams extends CommonAuthHookParams {
  /** The URL of the OAuth provider that Wasp will redirect the user to. */
  url: URL;
  /** The OAuth flow's data. */
  oauth: {
    /**
     * The unique request ID of the OAuth flow (you might know it as the
     * `state` parameter in OAuth).
     *
     * You can use it to save data (e.g. request query params) that you can
     * later use in the `onAfterSignup` or `onAfterLogin` hooks.
     */
    uniqueRequestId: string;
  };
}

/** The input of the {@link OnBeforeLoginHook | `onBeforeLogin`} hook. */
interface OnBeforeLoginHookParams extends CommonAuthHookParams {
  /** The auth identity the user is logging in with. */
  providerId: ProviderId;
  /** The user that is trying to log in. */
  user: UserEntity;
}

/** The input of the {@link OnAfterLoginHook | `onAfterLogin`} hook. */
interface OnAfterLoginHookParams extends CommonAuthHookParams {
  /** The auth identity the user logged in with. */
  providerId: ProviderId;
  /** The logged-in user. */
  user: UserEntity;
  /**
   * The OAuth flow's data. Only present if the user logged in with
   * [social auth](https://wasp.sh/docs/features/auth/social-auth/overview).
   */
  oauth?: OAuthData;
}

/**
 * The type of the `onBeforeSignup` auth hook, which Wasp calls before it
 * creates the user.
 *
 * Use it to reject a user based on some criteria before they sign up, by
 * throwing an `HttpError`.
 *
 * Works with all auth methods. Wasp awaits the hook if it's async, and
 * ignores its return value.
 *
 * @example
 * ```ts
 * import { HttpError } from "wasp/server";
 * import type { OnBeforeSignupHook } from "wasp/server/auth";
 *
 * export const onBeforeSignup: OnBeforeSignupHook = async ({
 *   providerId,
 *   prisma,
 *   req,
 * }) => {
 *   const count = await prisma.user.count();
 *   if (count > 100) {
 *     throw new HttpError(403, "Too many users");
 *   }
 *
 *   if (
 *     providerId.providerName === "email" &&
 *     providerId.providerUserId === "some@email.com"
 *   ) {
 *     throw new HttpError(403, "This email is not allowed");
 *   }
 * };
 * ```
 *
 * @see [Executing code before the user signs up](https://wasp.sh/docs/features/auth/hooks#executing-code-before-the-user-signs-up)
 */
export type OnBeforeSignupHook = (
  params: OnBeforeSignupHookParams,
) => void | Promise<void>;

/**
 * The type of the `onAfterSignup` auth hook, which Wasp calls after it
 * creates the user.
 *
 * Use it to send the user a welcome email, sync the user with a third-party
 * service, or store the OAuth access and refresh tokens in your database.
 *
 * Works with all auth methods. Wasp awaits the hook if it's async, and
 * ignores its return value.
 *
 * @example
 * ```ts
 * import type { OnAfterSignupHook } from "wasp/server/auth";
 *
 * export const onAfterSignup: OnAfterSignupHook = async ({
 *   providerId,
 *   user,
 *   oauth,
 *   prisma,
 *   req,
 * }) => {
 *   console.log("user object", user);
 *
 *   // If this is an OAuth signup, you have access to the OAuth tokens and the uniqueRequestId
 *   if (oauth) {
 *     console.log("accessToken", oauth.tokens.accessToken);
 *     console.log("uniqueRequestId", oauth.uniqueRequestId);
 *   }
 * };
 * ```
 *
 * @see [Executing code after the user signs up](https://wasp.sh/docs/features/auth/hooks#executing-code-after-the-user-signs-up)
 */
export type OnAfterSignupHook = (
  params: OnAfterSignupHookParams,
) => void | Promise<void>;

/**
 * The type of the `onAfterEmailVerified` auth hook, which Wasp calls exactly
 * once, after a user verifies their email.
 *
 * Use it to react to the verification, e.g. by sending a welcome email or
 * syncing the user with a third-party service.
 *
 * Only works with [Email](https://wasp.sh/docs/features/auth/email/overview)
 * auth. Wasp awaits the hook if it's async, and ignores its return value.
 *
 * @example
 * ```ts
 * import type { OnAfterEmailVerifiedHook } from "wasp/server/auth";
 * import { emailSender } from "wasp/server/email";
 *
 * export const onAfterEmailVerified: OnAfterEmailVerifiedHook = async ({
 *   email,
 * }) => {
 *   await emailSender.send({
 *     to: email,
 *     subject: "Thank you for verifying your email!",
 *     text: `Your email ${email} has been successfully verified!`,
 *     html: `<p>Your email ${email} has been successfully verified!</p>`,
 *   });
 * };
 * ```
 *
 * @see [Executing code after a user verifies their email](https://wasp.sh/docs/features/auth/hooks#executing-code-after-a-user-verifies-their-email)
 */
export type OnAfterEmailVerifiedHook = (
  params: OnAfterEmailVerifiedHookParams,
) => void | Promise<void>;

/**
 * The type of the `onBeforeOAuthRedirect` auth hook, which Wasp calls after
 * it generates the OAuth redirect URL, but before it redirects the user to
 * the OAuth provider.
 *
 * Use it to save data (e.g. request query parameters) that you can use later
 * in the OAuth flow, in the `onAfterSignup` or `onAfterLogin` hooks, through
 * the `uniqueRequestId`.
 *
 * Only works with
 * [social auth](https://wasp.sh/docs/features/auth/social-auth/overview).
 * Wasp awaits the hook if it's async.
 *
 * @returns An object with the URL that Wasp redirects the user to, e.g. the
 * `url` the hook received.
 *
 * @example
 * ```ts
 * import type { OnBeforeOAuthRedirectHook } from "wasp/server/auth";
 *
 * export const onBeforeOAuthRedirect: OnBeforeOAuthRedirectHook = async ({
 *   url,
 *   oauth,
 *   prisma,
 *   req,
 * }) => {
 *   // Saving query params for later use in onAfterSignup or onAfterLogin hooks
 *   const id = oauth.uniqueRequestId;
 *   someKindOfStore.set(id, req.query);
 *
 *   return { url };
 * };
 * ```
 *
 * @see [Executing code before the OAuth redirect](https://wasp.sh/docs/features/auth/hooks#executing-code-before-the-oauth-redirect)
 */
export type OnBeforeOAuthRedirectHook = (
  params: OnBeforeOAuthRedirectHookParams,
) => { url: URL } | Promise<{ url: URL }>;

/**
 * The type of the `onBeforeLogin` auth hook, which Wasp calls before it logs
 * the user in.
 *
 * Use it to reject a user based on some criteria before they log in, by
 * throwing an `HttpError`.
 *
 * Works with all auth methods. Wasp awaits the hook if it's async, and
 * ignores its return value.
 *
 * @example
 * ```ts
 * import { HttpError } from "wasp/server";
 * import type { OnBeforeLoginHook } from "wasp/server/auth";
 *
 * export const onBeforeLogin: OnBeforeLoginHook = async ({
 *   providerId,
 *   user,
 *   prisma,
 *   req,
 * }) => {
 *   if (
 *     providerId.providerName === "email" &&
 *     providerId.providerUserId === "some@email.com"
 *   ) {
 *     throw new HttpError(403, "You cannot log in with this email");
 *   }
 * };
 * ```
 *
 * @see [Executing code before the user logs in](https://wasp.sh/docs/features/auth/hooks#executing-code-before-the-user-logs-in)
 */
export type OnBeforeLoginHook = (
  params: OnBeforeLoginHookParams,
) => void | Promise<void>;

/**
 * The type of the `onAfterLogin` auth hook, which Wasp calls after the user
 * logs in.
 *
 * Use it to sync the user with a third-party service, or to update or
 * refresh the user's OAuth access token.
 *
 * Works with all auth methods. Wasp awaits the hook if it's async, and
 * ignores its return value.
 *
 * When using social auth, Wasp calls it right before creating the session,
 * which shouldn't make any difference in practice.
 *
 * @example
 * ```ts
 * import type { OnAfterLoginHook } from "wasp/server/auth";
 *
 * export const onAfterLogin: OnAfterLoginHook = async ({
 *   providerId,
 *   user,
 *   oauth,
 *   prisma,
 *   req,
 * }) => {
 *   console.log("user object", user);
 *
 *   // If this is an OAuth login, you have access to the OAuth tokens and the uniqueRequestId
 *   if (oauth) {
 *     console.log("accessToken", oauth.tokens.accessToken);
 *     console.log("uniqueRequestId", oauth.uniqueRequestId);
 *   }
 * };
 * ```
 *
 * @see [Executing code after the user logs in](https://wasp.sh/docs/features/auth/hooks#executing-code-after-the-user-logs-in)
 */
export type OnAfterLoginHook = (
  params: OnAfterLoginHookParams,
) => void | Promise<void>;

// OAuth

/** The OAuth tokens Google returns. */
interface GoogleTokens {
  accessToken: string;
  refreshToken: string | null;
  accessTokenExpiresAt: Date;
  idToken: string;
}

/** The OAuth tokens Google returns when refreshing the access token. */
interface GoogleRefreshedTokens {
  accessToken: string;
  accessTokenExpiresAt: Date;
}

/** The OAuth tokens GitHub returns. */
interface GitHubTokens {
  accessToken: string;
}

/** The OAuth tokens Discord returns. */
interface DiscordTokens {
  accessToken: string;
  refreshToken: string;
  accessTokenExpiresAt: Date;
}

/** The OAuth tokens Slack returns. */
interface SlackTokens {
  accessToken: string;
  idToken: string;
}

/** The OAuth tokens Keycloak returns. */
interface KeycloakTokens {
  accessToken: string;
  accessTokenExpiresAt: Date;
  refreshToken: string | null;
  refreshTokenExpiresAt: Date | null;
  idToken: string;
}

/** The OAuth tokens Microsoft Entra ID returns. */
interface MicrosoftEntraIdTokens {
  idToken: string;
  accessToken: string;
  accessTokenExpiresAt: Date;
  refreshToken: string | null;
}

/**
 * The data of the OAuth flow, which Wasp passes to the `onAfterSignup` and
 * `onAfterLogin` hooks when the user authenticates with
 * [social auth](https://wasp.sh/docs/features/auth/social-auth/overview).
 *
 * It has the following fields:
 *
 * - `uniqueRequestId`: the unique request ID of the OAuth flow (you might know
 *   it as the `state` parameter in OAuth). You can use it to get the data you
 *   saved in the `onBeforeOAuthRedirect` hook.
 * - `providerName`: the name of the OAuth provider the user authenticated
 *   with (e.g. `"google"`, `"github"`). It's one of the social auth providers
 *   your app enables.
 * - `tokens`: the OAuth tokens, which you can use to make requests to the
 *   provider's API on the user's behalf. Depending on the OAuth provider, the
 *   `tokens` object has different fields. For example, Google's has
 *   `accessToken`, `refreshToken`, `idToken`, and `accessTokenExpiresAt`.
 *
 * @example
 * To access the provider-specific token fields, first narrow down the
 * `oauth` object's type by checking its `providerName`:
 *
 * ```ts
 * if (oauth && oauth.providerName === "google") {
 *   console.log(oauth.tokens.accessToken);
 *   //                  ^ Google specific tokens are available here
 *   console.log(oauth.tokens.refreshToken);
 *   console.log(oauth.tokens.idToken);
 *   console.log(oauth.tokens.accessTokenExpiresAt);
 * }
 * ```
 *
 * @see [Refreshing the OAuth access token](https://wasp.sh/docs/features/auth/hooks#refreshing-the-oauth-access-token)
 */
export type OAuthData = {
  /**
   * The unique request ID of the OAuth flow (you might know it as the `state`
   * parameter in OAuth).
   */
  uniqueRequestId: string;
} & (
  | { providerName: "google"; tokens: GoogleTokens }
  | { providerName: "discord"; tokens: DiscordTokens }
  | { providerName: "github"; tokens: GitHubTokens }
  | { providerName: "keycloak"; tokens: KeycloakTokens }
  | { providerName: "microsoft"; tokens: MicrosoftEntraIdTokens }
  | { providerName: "slack"; tokens: SlackTokens }
);

/**
 * A social auth provider, with the OAuth client Wasp uses for it.
 */
interface OAuthProvider<Id extends string, OAuthClient> {
  /** The provider's ID, e.g. `"google"`. */
  id: Id;
  /** The provider's name, e.g. `"Google"`. */
  displayName: string;
  /**
   * The provider's OAuth client, from the [Arctic](https://arcticjs.dev/)
   * library. You can use it e.g. to refresh the user's access token.
   */
  oAuthClient: OAuthClient;
}

/** The parameters for creating an authorization URL. */
interface AuthorizationURLOptions {
  /** The OAuth scopes to request. */
  scopes?: string[];
}

/** Arctic's OAuth client for Google. */
interface GoogleOAuthClient {
  createAuthorizationURL(
    state: string,
    codeVerifier: string,
    options?: AuthorizationURLOptions,
  ): Promise<URL>;
  validateAuthorizationCode(
    code: string,
    codeVerifier: string,
  ): Promise<GoogleTokens>;
  refreshAccessToken(refreshToken: string): Promise<GoogleRefreshedTokens>;
}

/** Arctic's OAuth client for GitHub. */
interface GitHubOAuthClient {
  createAuthorizationURL(
    state: string,
    options?: AuthorizationURLOptions,
  ): Promise<URL>;
  validateAuthorizationCode(code: string): Promise<GitHubTokens>;
}

/** Arctic's OAuth client for Discord. */
interface DiscordOAuthClient {
  createAuthorizationURL(
    state: string,
    options?: AuthorizationURLOptions,
  ): Promise<URL>;
  validateAuthorizationCode(code: string): Promise<DiscordTokens>;
  refreshAccessToken(refreshToken: string): Promise<DiscordTokens>;
}

/** Arctic's OAuth client for Slack. */
interface SlackOAuthClient {
  createAuthorizationURL(
    state: string,
    options?: AuthorizationURLOptions,
  ): Promise<URL>;
  validateAuthorizationCode(code: string): Promise<SlackTokens>;
}

/** Arctic's OAuth client for Keycloak. */
interface KeycloakOAuthClient {
  createAuthorizationURL(
    state: string,
    codeVerifier: string,
    options?: AuthorizationURLOptions,
  ): Promise<URL>;
  validateAuthorizationCode(
    code: string,
    codeVerifier: string,
  ): Promise<KeycloakTokens>;
  refreshAccessToken(refreshToken: string): Promise<KeycloakTokens>;
}

/** Arctic's OAuth client for Microsoft Entra ID. */
interface MicrosoftEntraIdOAuthClient {
  createAuthorizationURL(
    state: string,
    codeVerifier: string,
    options?: AuthorizationURLOptions,
  ): Promise<URL>;
  validateAuthorizationCode(
    code: string,
    codeVerifier: string,
  ): Promise<MicrosoftEntraIdTokens>;
  refreshAccessToken(refreshToken: string): Promise<MicrosoftEntraIdTokens>;
}

/**
 * The [Google](https://wasp.sh/docs/features/auth/social-auth/google) auth
 * provider, with the OAuth client Wasp uses for it.
 *
 * Only available with Google auth.
 *
 * @example
 * Refreshing the user's access token:
 *
 * ```ts
 * import type { OnAfterLoginHook } from "wasp/server/auth";
 * import { google } from "wasp/server/auth";
 *
 * export const onAfterLogin: OnAfterLoginHook = async ({ oauth }) => {
 *   if (
 *     oauth &&
 *     oauth.providerName === "google" &&
 *     oauth.tokens.refreshToken !== null
 *   ) {
 *     const newTokens = await google.oAuthClient.refreshAccessToken(
 *       oauth.tokens.refreshToken,
 *     );
 *     console.log("new tokens", newTokens);
 *   }
 * };
 * ```
 *
 * @see [Refreshing the OAuth access token](https://wasp.sh/docs/features/auth/hooks#refreshing-the-oauth-access-token)
 */
export declare const google: OAuthProvider<"google", GoogleOAuthClient>;

/**
 * The [GitHub](https://wasp.sh/docs/features/auth/social-auth/github) auth
 * provider, with the OAuth client Wasp uses for it.
 *
 * Only available with GitHub auth.
 */
export declare const github: OAuthProvider<"github", GitHubOAuthClient>;

/**
 * The [Discord](https://wasp.sh/docs/features/auth/social-auth/discord) auth
 * provider, with the OAuth client Wasp uses for it.
 *
 * Only available with Discord auth.
 *
 * @see [Refreshing the OAuth access token](https://wasp.sh/docs/features/auth/hooks#refreshing-the-oauth-access-token)
 */
export declare const discord: OAuthProvider<"discord", DiscordOAuthClient>;

/**
 * The [Slack](https://wasp.sh/docs/features/auth/social-auth/slack) auth
 * provider, with the OAuth client Wasp uses for it.
 *
 * Only available with Slack auth.
 */
export declare const slack: OAuthProvider<"slack", SlackOAuthClient>;

/**
 * The [Keycloak](https://wasp.sh/docs/features/auth/social-auth/keycloak)
 * auth provider, with the OAuth client Wasp uses for it.
 *
 * Only available with Keycloak auth.
 *
 * @see [Refreshing the OAuth access token](https://wasp.sh/docs/features/auth/hooks#refreshing-the-oauth-access-token)
 */
export declare const keycloak: OAuthProvider<"keycloak", KeycloakOAuthClient>;

/**
 * The [Microsoft](https://wasp.sh/docs/features/auth/social-auth/microsoft)
 * auth provider, with the OAuth client Wasp uses for it.
 *
 * Only available with Microsoft auth.
 *
 * @see [Refreshing the OAuth access token](https://wasp.sh/docs/features/auth/hooks#refreshing-the-oauth-access-token)
 */
export declare const microsoft: OAuthProvider<
  "microsoft",
  MicrosoftEntraIdOAuthClient
>;

export {};
