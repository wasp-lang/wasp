/**
 * Auth APIs you can use both on the client and on the server: the
 * {@link AuthUser} type of the logged-in user, and helpers for reading auth
 * data from a `User` entity object.
 *
 * Available when your app enables [auth](https://wasp.sh/docs/features/auth/overview).
 *
 * @module
 */

import type { EmailProviderData } from "@wasp.sh/lib-sdk-core";

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
 * `identities` relation, included.
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
    identities: {
      /** The auth method's name, e.g. `"email"` or `"google"`. */
      providerName: string;
      /** The user's ID for that auth method, e.g. their email or Google ID. */
      providerUserId: string;
    }[];
  } | null;
};

/**
 * The auth data of each auth method the user signed up with.
 *
 * There's one property for each auth method your app enables. Its value is
 * `null` if the user doesn't have an identity for that auth method, so check
 * it before using it.
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
   * auth data. Only present if your app enables email auth.
   */
  email?:
    | ({
        /** The user's email address. */
        id: string;
      } & Omit<EmailProviderData, "hashedPassword">)
    | null;
  /**
   * The user's
   * [Username & password](https://wasp.sh/docs/features/auth/username-and-pass/overview)
   * auth data. Only present if your app enables username and password auth.
   */
  username?: {
    /** The user's username. */
    id: string;
  } | null;
  /**
   * The user's [Google](https://wasp.sh/docs/features/auth/social-auth/google)
   * auth data. Only present if your app enables Google auth.
   */
  google?: {
    /** The user's Google ID. */
    id: string;
  } | null;
  /**
   * The user's [GitHub](https://wasp.sh/docs/features/auth/social-auth/github)
   * auth data. Only present if your app enables GitHub auth.
   */
  github?: {
    /** The user's GitHub ID. */
    id: string;
  } | null;
  /**
   * The user's [Discord](https://wasp.sh/docs/features/auth/social-auth/discord)
   * auth data. Only present if your app enables Discord auth.
   */
  discord?: {
    /** The user's Discord ID. */
    id: string;
  } | null;
  /**
   * The user's [Slack](https://wasp.sh/docs/features/auth/social-auth/slack)
   * auth data. Only present if your app enables Slack auth.
   */
  slack?: {
    /** The user's Slack ID. */
    id: string;
  } | null;
  /**
   * The user's [Keycloak](https://wasp.sh/docs/features/auth/social-auth/keycloak)
   * auth data. Only present if your app enables Keycloak auth.
   */
  keycloak?: {
    /** The user's Keycloak ID. */
    id: string;
  } | null;
  /**
   * The user's
   * [Microsoft](https://wasp.sh/docs/features/auth/social-auth/microsoft)
   * auth data. Only present if your app enables Microsoft auth.
   */
  microsoft?: {
    /** The user's Microsoft ID. */
    id: string;
  } | null;
}

/**
 * The logged-in user, as you receive it on the client (from
 * [`useAuth`](https://wasp.sh/docs/api/wasp/client/auth/functions/useAuth) or
 * the `user` prop of pages with `authRequired`) and on the server (as
 * `context.user` in Operations).
 *
 * It has all the fields you defined on your user entity at the top level, and
 * the auth-related data of each auth method in the `identities` field. Wasp
 * strips the hashed passwords out of it.
 *
 * Its shape depends on your app: its fields come from your user entity, and
 * `identities` has a property for each auth method your app enables.
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
  /**
   * The auth data of each auth method the user signed up with.
   */
  identities: AuthUserIdentities;
  /**
   * Returns the first user ID that it finds for the user. For example, if the
   * user signed up with email, it returns their email. If they signed up with
   * Google, it returns their Google ID.
   *
   * This is useful if you support multiple auth methods and you need _any_ ID
   * that identifies the user in your app.
   *
   * @returns The user's ID for one of their auth methods, or `null` if they
   * don't have any.
   */
  getFirstProviderUserId: () => string | null;
};

/**
 * Returns the user's email if they signed up with
 * [Email](https://wasp.sh/docs/features/auth/email/overview) auth.
 *
 * Works both on the client and on the server.
 *
 * @param user A user entity object with the `auth` and `identities` relations
 * included.
 * @returns The user's email, or `null` if they didn't sign up with email.
 *
 * @example
 * ```tsx
 * import { getEmail } from "wasp/auth";
 *
 * function MainPage() {
 *   // ...
 *   return (
 *     <div className="tasks">
 *       {tasks.map((task) => (
 *         <div key={task.id} className="task">
 *           {task.title} by {getEmail(task.user)}
 *         </div>
 *       ))}
 *     </div>
 *   );
 * }
 * ```
 *
 * @see [Getting auth data from the user object](https://wasp.sh/docs/features/auth/entities#getting-auth-data-from-the-user-object)
 */
export declare function getEmail(user: UserEntityWithAuth): string | null;

/**
 * Returns the user's username if they signed up with
 * [Username & password](https://wasp.sh/docs/features/auth/username-and-pass/overview)
 * auth.
 *
 * Works both on the client and on the server.
 *
 * @param user A user entity object with the `auth` and `identities` relations
 * included.
 * @returns The user's username, or `null` if they didn't sign up with a
 * username.
 *
 * @example
 * ```tsx
 * import { getUsername } from "wasp/auth";
 *
 * function MainPage() {
 *   // ...
 *   return (
 *     <div className="tasks">
 *       {tasks.map((task) => (
 *         <div key={task.id} className="task">
 *           {task.title} by {getUsername(task.user)}
 *         </div>
 *       ))}
 *     </div>
 *   );
 * }
 * ```
 *
 * @see [Getting auth data from the user object](https://wasp.sh/docs/features/auth/entities#getting-auth-data-from-the-user-object)
 */
export declare function getUsername(user: UserEntityWithAuth): string | null;

/**
 * Returns the first user ID that it finds for the user. For example, if the
 * user signed up with email, it returns their email. If they signed up with
 * Google, it returns their Google ID.
 *
 * Works both on the client and on the server. If you have an
 * {@link AuthUser}, you can call its `getFirstProviderUserId` method instead.
 *
 * @param user A user entity object with the `auth` and `identities` relations
 * included.
 * @returns The user's ID for one of their auth methods, or `null` if you
 * don't pass a user or they don't have any.
 *
 * @example
 * ```tsx
 * import { getFirstProviderUserId } from "wasp/auth";
 *
 * function MainPage() {
 *   // ...
 *   return (
 *     <div className="tasks">
 *       {tasks.map((task) => (
 *         <div key={task.id} className="task">
 *           {task.title} by {getFirstProviderUserId(task.user)}
 *         </div>
 *       ))}
 *     </div>
 *   );
 * }
 * ```
 *
 * @see [Getting auth data from the user object](https://wasp.sh/docs/features/auth/entities#getting-auth-data-from-the-user-object)
 */
export declare function getFirstProviderUserId(
  user?: UserEntityWithAuth,
): string | null;

export {};
