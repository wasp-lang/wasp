/**
 * Types of the extra signup fields you define for your auth methods.
 *
 * When you define `userSignupFields` for an auth method (see
 * [Customizing the signup process](https://wasp.sh/docs/features/auth/overview#customizing-the-signup-process)),
 * Wasp infers the type of the extra fields from it and exports it from this
 * module. Each type only exists if you define `userSignupFields` for its auth
 * method.
 *
 * @module
 */

/**
 * The extra signup fields of the
 * [Email](https://wasp.sh/docs/features/auth/email/overview) auth method.
 *
 * It's an object with one property for each field you defined in
 * `auth.methods.email.userSignupFields`, typed with the return type of that
 * field's function. It's also part of the input of the email
 * [`signup`](https://wasp.sh/docs/api/wasp/client/auth/functions/signup)
 * function from `wasp/client/auth`.
 *
 * Wasp only exports this type if you define
 * `auth.methods.email.userSignupFields`.
 *
 * @example
 * If you define:
 *
 * ```ts
 * export const userSignupFields = defineUserSignupFields({
 *   address: (data) => {
 *     if (typeof data.address !== "string") {
 *       throw new Error("Address is required");
 *     }
 *     return data.address;
 *   },
 * });
 * ```
 *
 * `UserEmailSignupFields` is `{ address: string }`.
 */
export type UserEmailSignupFields = { [field: string]: unknown };

/**
 * The extra signup fields of the
 * [Username & password](https://wasp.sh/docs/features/auth/username-and-pass/overview)
 * auth method.
 *
 * It's an object with one property for each field you defined in
 * `auth.methods.usernameAndPassword.userSignupFields`, typed with the return
 * type of that field's function. It's also part of the input of the username
 * and password
 * [`signup`](https://wasp.sh/docs/api/wasp/client/auth/functions/signup)
 * function from `wasp/client/auth`.
 *
 * Wasp only exports this type if you define
 * `auth.methods.usernameAndPassword.userSignupFields`.
 */
export type UserUsernameAndPasswordSignupFields = { [field: string]: unknown };
