/**
 * Auth APIs you can use both on the client and on the server: the
 * {@link AuthUser} type of the logged-in user, and helpers for
 * [reading auth data from a user entity object](https://wasp.sh/docs/features/auth/entities#getting-auth-data-from-the-user-object),
 * and {@link definePasswordValidationSchema} for setting your own password
 * rules.
 *
 * Only available if your app has
 * [auth](https://wasp.sh/docs/features/auth/overview) enabled.
 *
 * @module
 */

/**
 * Your app's user entity: the Prisma model you set as `auth.userEntity` in
 * your app spec (usually `User`).
 *
 * Wasp generates the real type from your Prisma schema, so it has the fields
 * you defined on that model.
 */
interface UserEntity {}

/**
 * A user entity object fetched with its `auth` relation, and the `auth`'s
 * `identities` relation, included. An {@link AuthUser} doesn't have the `auth`
 * relation, so read its `identities` field instead.
 *
 * For example, the `user` field of a `Task` you fetched with:
 *
 * ```ts
 * const tasks = await context.entities.Task.findMany({
 *   include: {
 *     user: {
 *       include: {
 *         auth: {
 *           include: {
 *             identities: {
 *               select: { providerName: true, providerUserId: true },
 *             },
 *           },
 *         },
 *       },
 *     },
 *   },
 * });
 * ```
 *
 * @see [Including the user with other entities](https://wasp.sh/docs/features/auth/entities#including-the-user-with-other-entities)
 */
type UserEntityWithAuth = UserEntity & {
  auth: {
    identities: UserEntityAuthIdentity[];
  } | null;
};

/** One of the auth identities in a {@link UserEntityWithAuth}. */
interface UserEntityAuthIdentity {
  /** The auth method's name, e.g. `"email"` or `"google"`. */
  providerName: string;
  /** The user's ID for that auth method, e.g. their email or Google ID. */
  providerUserId: string;
}

/**
 * The user's auth data. It only has properties for the auth methods your app
 * enables. A property's value is `null` if the user doesn't have an identity
 * for that auth method, so check it before using it.
 *
 * @example
 * ```ts
 * if (user.identities.email !== null) {
 *   const email = user.identities.email.id;
 *   // ...
 * } else if (user.identities.google !== null) {
 *   const googleId = user.identities.google.id;
 *   // ...
 * }
 * ```
 */
interface AuthUserIdentities {
  /**
   * The user's [Email](https://wasp.sh/docs/features/auth/email/overview)
   * auth data.
   */
  email?: {
    /** The user's email address. */
    id: string;
    /** `true` if the user has verified their email address. */
    isEmailVerified: boolean;
    /** Datetime when the email verification email was sent. */
    emailVerificationSentAt: string | null;
    /** Datetime when the last password reset email was sent. */
    passwordResetSentAt: string | null;
  } | null;
  /**
   * The user's
   * [Username & password](https://wasp.sh/docs/features/auth/username-and-pass/overview)
   * auth data.
   */
  username?: {
    /** The user's username. */
    id: string;
  } | null;
  /**
   * The user's [Google](https://wasp.sh/docs/features/auth/social-auth/google)
   * auth data.
   */
  google?: {
    /** The user's Google ID. */
    id: string;
  } | null;
  /**
   * The user's [GitHub](https://wasp.sh/docs/features/auth/social-auth/github)
   * auth data.
   */
  github?: {
    /** The user's GitHub ID. */
    id: string;
  } | null;
  /**
   * The user's [Discord](https://wasp.sh/docs/features/auth/social-auth/discord)
   * auth data.
   */
  discord?: {
    /** The user's Discord ID. */
    id: string;
  } | null;
  /**
   * The user's [Slack](https://wasp.sh/docs/features/auth/social-auth/slack)
   * auth data.
   */
  slack?: {
    /** The user's Slack ID. */
    id: string;
  } | null;
  /**
   * The user's [Keycloak](https://wasp.sh/docs/features/auth/social-auth/keycloak)
   * auth data.
   */
  keycloak?: {
    /** The user's Keycloak ID. */
    id: string;
  } | null;
  /**
   * The user's
   * [Microsoft](https://wasp.sh/docs/features/auth/social-auth/microsoft)
   * auth data.
   */
  microsoft?: {
    /** The user's Microsoft ID. */
    id: string;
  } | null;
}

/**
 * The logged-in user, as you receive it on the client (from
 * {@link "client/auth"!useAuth | useAuth} or
 * the `user` prop of pages with `authRequired`) and on the server (as
 * `context.user` in Operations and APIs, and `socket.data.user` in
 * WebSockets).
 *
 * Its shape depends on your app: it has all the fields you defined on your
 * user entity at the top level, and the auth-related data of each auth method
 * your app enables in the `identities` field. Wasp strips the hashed passwords
 * out of it.
 *
 * @example
 * For a user that signed up with email, in an app with email and Google
 * auth, it looks like this:
 *
 * ```ts
 * const user = {
 *   // User data
 *   id: "cluqsex9500017cn7i2hwsg17",
 *   address: "Some address",
 *
 *   // Auth methods specific data
 *   identities: {
 *     email: {
 *       id: "user@app.com",
 *       isEmailVerified: true,
 *       emailVerificationSentAt: "2024-04-08T10:06:02.204Z",
 *       passwordResetSentAt: null,
 *     },
 *     google: null,
 *   },
 * };
 * ```
 *
 * @example
 * Typing the `user` prop of a page with `authRequired`:
 *
 * ```tsx
 * import type { AuthUser } from "wasp/auth";
 *
 * export const AccountPage = ({ user }: { user: AuthUser }) => {
 *   const userId = user.getFirstProviderUserId();
 *   // ...
 * };
 * ```
 *
 * @see [Accessing the logged-in user](https://wasp.sh/docs/features/auth/overview#accessing-the-logged-in-user)
 * @see [Accessing user data](https://wasp.sh/docs/features/auth/entities#accessing-the-auth-fields)
 */
export type AuthUser = UserEntity & {
  identities: AuthUserIdentities;
  /**
   * Returns the first user ID that it finds for the user, or `null` if they
   * don't have any. For example, if the user signed up with email, it returns
   * their email. If they signed up with Google, it returns their Google ID.
   *
   * This is useful if you support multiple auth methods and you need _any_ ID
   * that identifies the user in your app.
   */
  getFirstProviderUserId: () => string | null;
};

/**
 * Returns the user's email if they signed up with
 * [Email](https://wasp.sh/docs/features/auth/email/overview) auth, or `null`
 * otherwise.
 *
 * @example
 * ```ts
 * import { getEmail } from "wasp/auth";
 *
 * const email = getEmail(task.user);
 * ```
 */
export declare function getEmail(user: UserEntityWithAuth): string | null;

/**
 * Returns the user's username if they signed up with
 * [Username & password](https://wasp.sh/docs/features/auth/username-and-pass/overview)
 * auth, or `null` otherwise.
 *
 * @example
 * ```ts
 * import { getUsername } from "wasp/auth";
 *
 * const username = getUsername(task.user);
 * ```
 */
export declare function getUsername(user: UserEntityWithAuth): string | null;

/**
 * Returns the user's ID for one of their auth methods, or `null` if you don't
 * pass a user or they don't have any.
 *
 * Works like the `getFirstProviderUserId` method of {@link AuthUser}, which
 * you can call instead if you have one.
 *
 * @example
 * ```ts
 * import { getFirstProviderUserId } from "wasp/auth";
 *
 * const userId = getFirstProviderUserId(task.user);
 * ```
 */
export declare function getFirstProviderUserId(
  user?: UserEntityWithAuth,
): string | null;

/**
 * Defines a Zod schema that new passwords must pass on signup and password
 * reset, replacing Wasp's default password rules.
 *
 * It returns the schema unchanged, and only exists to make sure the schema
 * validates a string. Export the schema from a file in `src/` and pass it to
 * the `passwordValidationSchema` field of the `email` or `usernameAndPassword`
 * auth method in the app spec.
 *
 * @param schema A Zod schema for a string.
 *
 * @example
 * ```ts
 * // src/auth/password.ts
 * import * as z from "zod";
 * import { definePasswordValidationSchema } from "wasp/auth";
 *
 * export const passwordValidationSchema = definePasswordValidationSchema(
 *   z
 *     .string()
 *     .min(12, "Password must be at least 12 characters")
 *     .regex(/\p{Lu}/u, "Password must contain an uppercase letter"),
 * );
 * ```
 *
 * @see [Custom password rules](https://wasp.sh/docs/features/auth/overview#custom-password-rules)
 */
export { definePasswordValidationSchema } from "@wasp/lib-sdk-core";

export {};
