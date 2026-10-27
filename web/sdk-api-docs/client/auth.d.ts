/**
 * Client-side auth APIs: getting the logged-in user, logging out, the
 * generated [Auth UI](https://wasp.sh/docs/features/auth/ui) components, and
 * the functions and helpers for building your own auth UI.
 *
 * Available when your app enables
 * [auth](https://wasp.sh/docs/features/auth/overview). Some exports depend on
 * the auth methods your app enables:
 *
 * - With [Email](https://wasp.sh/docs/features/auth/email/overview) auth:
 *   the email versions of {@link login} and {@link signup}, and
 *   {@link requestPasswordReset}, {@link resetPassword}, {@link verifyEmail},
 *   {@link ForgotPasswordForm}, {@link ResetPasswordForm}, and
 *   {@link VerifyEmailForm}.
 * - With
 *   [Username & password](https://wasp.sh/docs/features/auth/username-and-pass/overview)
 *   auth: the username versions of {@link login} and {@link signup}.
 * - With a [social auth](https://wasp.sh/docs/features/auth/social-auth/overview)
 *   provider: that provider's sign-in URL and button, e.g.
 *   {@link googleSignInUrl} and {@link GoogleSignInButton} for Google.
 *
 * @module
 */

import type { UseQueryResult } from "@tanstack/react-query";
import type { Query } from "@wasp.sh/lib-sdk-core";
import type {
  AdditionalSignupFields,
  CustomizationOptions,
} from "@wasp.sh/lib-sdk-core/browser";
import type { JSX } from "react";
import type { AuthUser } from "../auth";
import type {
  UserEmailSignupFields,
  UserUsernameAndPasswordSignupFields,
} from "../auth/providers";

// Logged-in user

/**
 * Gets the logged-in user from a React component, and keeps it up to date.
 *
 * It's a thin wrapper over the
 * [`useQuery`](https://wasp.sh/docs/api/wasp/client/operations/functions/useQuery)
 * hook (calling the {@link getMe} Query), and returns data in the same format.
 *
 * You can use it in any page. In pages with `authRequired`, you can use the
 * `user` prop instead.
 *
 * @returns React Query's result object, whose `data` is the logged-in
 * {@link AuthUser}, or `null` if the user isn't logged in.
 *
 * @example
 * ```tsx
 * import { useAuth, logout } from "wasp/client/auth";
 * import { Link } from "react-router";
 * import Todo from "../Todo";
 *
 * export function Main() {
 *   const { data: user } = useAuth();
 *
 *   if (!user) {
 *     return (
 *       <span>
 *         Please <Link to="/login">login</Link> or{" "}
 *         <Link to="/signup">sign up</Link>.
 *       </span>
 *     );
 *   } else {
 *     return (
 *       <>
 *         <button onClick={logout}>Logout</button>
 *         <Todo />
 *       </>
 *     );
 *   }
 * }
 * ```
 *
 * @see [Getting the `user` in non-authenticated routes](https://wasp.sh/docs/features/auth/overview#getting-the-user-in-non-authenticated-routes)
 */
export declare function useAuth(): UseQueryResult<AuthUser | null>;

/**
 * The Query that {@link useAuth} uses to fetch the logged-in user.
 *
 * You can call it directly, or pass it to
 * [`useQuery`](https://wasp.sh/docs/api/wasp/client/operations/functions/useQuery).
 * It resolves to the logged-in {@link AuthUser}, or `null` if the user isn't
 * logged in.
 */
export declare const getMe: Query<void, AuthUser | null>;

/**
 * Logs the user out.
 *
 * It deletes the session from the server and the user's data from the
 * client, and invalidates all Queries. It removes the local data even if the
 * request to the server fails.
 *
 * @example
 * ```tsx
 * import { logout } from "wasp/client/auth";
 *
 * const LogoutButton = () => {
 *   return <button onClick={logout}>Logout</button>;
 * };
 * ```
 *
 * @see [Logout action](https://wasp.sh/docs/features/auth/overview#logout-action)
 */
export declare function logout(): Promise<void>;

// Auth UI

/**
 * A login form, which shows the login fields and buttons of all the auth
 * methods your app enables.
 *
 * Works with all auth methods.
 *
 * @param props Options to customize the form's look. See
 * {@link CustomizationOptions}.
 *
 * @example
 * ```tsx
 * import { LoginForm } from "wasp/client/auth";
 *
 * export function LoginPage() {
 *   return <LoginForm />;
 * }
 * ```
 *
 * @see [Auth UI](https://wasp.sh/docs/features/auth/ui#login-form)
 */
export declare function LoginForm(props: CustomizationOptions): JSX.Element;

/**
 * A signup form, which shows the signup fields and buttons of all the auth
 * methods your app enables.
 *
 * Works with all auth methods.
 *
 * @param props Options to customize the form's look (see
 * {@link CustomizationOptions}), and:
 *
 * - `additionalFields`: extra fields or UI to add to the form, e.g. for
 *   [extra signup fields](https://wasp.sh/docs/features/auth/overview#customizing-the-signup-process).
 *   It can be either a list of extra fields or a render function.
 *
 *   The list's items can be objects or render functions (you can combine
 *   them). Wasp adds them to the form one by one, in the order you pass them
 *   in:
 *
 *   1. Objects are a simple way to describe new fields you need, but a bit
 *      less flexible than render functions. They have the following
 *      properties:
 *
 *      - `name` (required): the name of the field.
 *      - `label` (required): the label of the field (used in the UI).
 *      - `type` (required): the type of the field, which can be `"input"` or
 *        `"textarea"`.
 *      - `validations`: an object with the validation rules for the field.
 *        The keys are the validation names, and the values are the
 *        validation error messages. Read more about the available validation
 *        rules in the
 *        [react-hook-form docs](https://react-hook-form.com/api/useform/register#register).
 *
 *   2. Render functions can render any UI you want, but they require a bit
 *      more code. They receive two arguments and return a React node:
 *
 *      - `form`: the `react-hook-form` object (read more about it in the
 *        [react-hook-form docs](https://react-hook-form.com/api/useform)).
 *        Use its `form.register` function to register your fields.
 *      - `state`: the form state object, which has an `isLoading: boolean`
 *        property that tells whether the form is currently submitting.
 *
 *   If you pass a single render function instead of a list, Wasp renders
 *   whatever it returns below the default fields.
 *
 * @example
 * Adding a field with an object and another with a render function:
 *
 * ```tsx
 * import {
 *   SignupForm,
 *   FormError,
 *   FormInput,
 *   FormItemGroup,
 *   FormLabel,
 * } from "wasp/client/auth";
 *
 * export const SignupPage = () => {
 *   return (
 *     <SignupForm
 *       additionalFields={[
 *         {
 *           name: "address",
 *           label: "Address",
 *           type: "input",
 *           validations: {
 *             required: "Address is required",
 *           },
 *         },
 *         (form, state) => {
 *           return (
 *             <FormItemGroup>
 *               <FormLabel>Phone Number</FormLabel>
 *               <FormInput
 *                 {...form.register("phoneNumber", {
 *                   required: "Phone number is required",
 *                 })}
 *                 disabled={state.isLoading}
 *               />
 *               {form.formState.errors.phoneNumber && (
 *                 <FormError>
 *                   {form.formState.errors.phoneNumber.message}
 *                 </FormError>
 *               )}
 *             </FormItemGroup>
 *           );
 *         },
 *       ]}
 *     />
 *   );
 * };
 * ```
 *
 * @example
 * Adding UI with a single render function:
 *
 * ```tsx
 * import { SignupForm, FormItemGroup } from "wasp/client/auth";
 *
 * export const SignupPage = () => {
 *   return (
 *     <SignupForm
 *       additionalFields={(form, state) => {
 *         const username = form.watch("username");
 *         return (
 *           username && (
 *             <FormItemGroup>
 *               Hello there <strong>{username}</strong> 👋
 *             </FormItemGroup>
 *           )
 *         );
 *       }}
 *     />
 *   );
 * };
 * ```
 *
 * @see [Auth UI](https://wasp.sh/docs/features/auth/ui#signup-form)
 * @see [Customizing the signup component](https://wasp.sh/docs/features/auth/overview#2-customizing-the-signup-component)
 */
export declare function SignupForm(
  props: CustomizationOptions & { additionalFields?: AdditionalSignupFields },
): JSX.Element;

/**
 * A form where users who forgot their password can request a password reset
 * email.
 *
 * Only available with [Email](https://wasp.sh/docs/features/auth/email/overview)
 * auth.
 *
 * @param props Options to customize the form's look. See
 * {@link CustomizationOptions}.
 *
 * @example
 * ```tsx
 * import { ForgotPasswordForm } from "wasp/client/auth";
 *
 * export function ForgotPasswordPage() {
 *   return <ForgotPasswordForm />;
 * }
 * ```
 *
 * @see [Auth UI](https://wasp.sh/docs/features/auth/ui#forgot-password-form)
 */
export declare function ForgotPasswordForm(
  props: CustomizationOptions,
): JSX.Element;

/**
 * A form where users set a new password, after following the link in the
 * password reset email.
 *
 * Only available with [Email](https://wasp.sh/docs/features/auth/email/overview)
 * auth. Render it in the page of the route you set as
 * `auth.methods.email.passwordReset.clientRoute`.
 *
 * @param props Options to customize the form's look. See
 * {@link CustomizationOptions}.
 *
 * @example
 * ```tsx
 * import { ResetPasswordForm } from "wasp/client/auth";
 *
 * export function ResetPasswordPage() {
 *   return <ResetPasswordForm />;
 * }
 * ```
 *
 * @see [Auth UI](https://wasp.sh/docs/features/auth/ui#reset-password-form)
 */
export declare function ResetPasswordForm(
  props: CustomizationOptions,
): JSX.Element;

/**
 * A form that verifies the user's email, after they follow the link in the
 * verification email they receive when they sign up.
 *
 * Only available with [Email](https://wasp.sh/docs/features/auth/email/overview)
 * auth. Render it in the page of the route you set as
 * `auth.methods.email.emailVerification.clientRoute`.
 *
 * @param props Options to customize the form's look. See
 * {@link CustomizationOptions}.
 *
 * @example
 * ```tsx
 * import { VerifyEmailForm } from "wasp/client/auth";
 *
 * export function VerifyEmailPage() {
 *   return <VerifyEmailForm />;
 * }
 * ```
 *
 * @see [Auth UI](https://wasp.sh/docs/features/auth/ui#verify-email-form)
 */
export declare function VerifyEmailForm(
  props: CustomizationOptions,
): JSX.Element;

/**
 * Props you can pass to all the Auth UI forms to customize their look:
 *
 * - `appearance`: customizes the form's colors and font sizes. The Auth UI
 *   uses CSS variables in its styling, and you can override the default theme
 *   tokens by setting them in `appearance.colors` (e.g. `brand`,
 *   `brandAccent`, `submitButtonText`) and `appearance.fontSizes`.
 * - `logo`: the path to your logo, which the form shows at the top.
 * - `socialLayout`: the layout of the social login buttons, which can be
 *   `"horizontal"` (the default) or `"vertical"`.
 *
 * @example
 * We recommend defining your appearance in a separate file and importing it
 * into your components:
 *
 * ```ts
 * // src/appearance.ts
 * import type { CustomizationOptions } from "wasp/client/auth";
 *
 * export const authAppearance: CustomizationOptions["appearance"] = {
 *   colors: {
 *     brand: "#5969b8", // blue
 *     brandAccent: "#de5998", // pink
 *     submitButtonText: "white",
 *   },
 * };
 * ```
 *
 * ```tsx
 * // src/LoginPage.tsx
 * import { LoginForm } from "wasp/client/auth";
 * import { authAppearance } from "./appearance";
 * import todoLogo from "./todoLogo.png";
 *
 * export function LoginPage() {
 *   return (
 *     <LoginForm
 *       appearance={authAppearance}
 *       logo={todoLogo}
 *       socialLayout="vertical"
 *     />
 *   );
 * }
 * ```
 *
 * @see [Customizing the Auth UI](https://wasp.sh/docs/features/auth/ui)
 */
export type { CustomizationOptions } from "@wasp.sh/lib-sdk-core/browser";

/**
 * A wrapper for a form field, which groups its label, input, and error.
 * Use it to build the extra fields you pass to {@link SignupForm}, so they
 * look like the rest of the form.
 *
 * @see [Using a list of extra fields](https://wasp.sh/docs/features/auth/overview#using-a-list-of-extra-fields)
 */
export { FormItemGroup } from "@wasp.sh/lib-sdk-core/browser";

/**
 * A form field label, styled like the rest of the Auth UI. Use it to build
 * the extra fields you pass to {@link SignupForm}.
 *
 * @see [Using a list of extra fields](https://wasp.sh/docs/features/auth/overview#using-a-list-of-extra-fields)
 */
export { FormLabel } from "@wasp.sh/lib-sdk-core/browser";

/**
 * A text input, styled like the rest of the Auth UI. Use it to build the extra
 * fields you pass to {@link SignupForm}.
 *
 * @see [Using a list of extra fields](https://wasp.sh/docs/features/auth/overview#using-a-list-of-extra-fields)
 */
export { FormInput } from "@wasp.sh/lib-sdk-core/browser";

/**
 * A textarea, styled like the rest of the Auth UI. Use it to build the extra
 * fields you pass to {@link SignupForm}.
 *
 * @see [Using a list of extra fields](https://wasp.sh/docs/features/auth/overview#using-a-list-of-extra-fields)
 */
export { FormTextarea } from "@wasp.sh/lib-sdk-core/browser";

/**
 * A form field error message, styled like the rest of the Auth UI. Use it to
 * build the extra fields you pass to {@link SignupForm}.
 *
 * @see [Using a list of extra fields](https://wasp.sh/docs/features/auth/overview#using-a-list-of-extra-fields)
 */
export { FormError } from "@wasp.sh/lib-sdk-core/browser";

// Email and username auth

/**
 * Logs the user in with [Email](https://wasp.sh/docs/features/auth/email/overview)
 * auth.
 *
 * Only available with Email auth. Make sure to redirect the user on success
 * (e.g. to the main page of the app).
 *
 * @param data The user's credentials.
 * @param data.email The user's email.
 * @param data.password The user's password.
 *
 * @example
 * ```ts
 * import { login } from "wasp/client/auth";
 *
 * try {
 *   await login({ email: email.trim(), password });
 *   navigate("/");
 * } catch (error) {
 *   setError(error);
 * }
 * ```
 *
 * @see [Create your own UI for email auth](https://wasp.sh/docs/features/auth/email/create-your-own-ui)
 */
export declare function login(data: {
  email: string;
  password: string;
}): Promise<void>;

/**
 * Logs the user in with
 * [Username & password](https://wasp.sh/docs/features/auth/username-and-pass/overview)
 * auth.
 *
 * Only available with Username & password auth. Make sure to redirect the
 * user on success (e.g. to the main page of the app).
 *
 * @param data The user's credentials.
 * @param data.username The user's username.
 * @param data.password The user's password.
 *
 * @example
 * ```ts
 * import { login } from "wasp/client/auth";
 *
 * try {
 *   await login({ username, password });
 *   navigate("/");
 * } catch (error) {
 *   setError(error);
 * }
 * ```
 *
 * @see [Create your own UI for username and password auth](https://wasp.sh/docs/features/auth/username-and-pass/create-your-own-ui)
 */
export declare function login(data: {
  username: string;
  password: string;
}): Promise<void>;

/**
 * Signs the user up with
 * [Email](https://wasp.sh/docs/features/auth/email/overview) auth, and sends
 * them the verification email.
 *
 * Only available with Email auth. It doesn't log the user in, since they
 * still need to verify their email.
 *
 * By default, Wasp only saves the `email` and `password` fields. If you
 * [define extra signup fields](https://wasp.sh/docs/features/auth/overview#customizing-the-signup-process),
 * pass them in `data` too.
 *
 * @param data The new user's data, including your extra signup fields.
 * @param data.email The user's email.
 * @param data.password The user's password.
 *
 * @example
 * ```ts
 * import { signup } from "wasp/client/auth";
 *
 * try {
 *   await signup({ email: email.trim(), password });
 *   setNeedsConfirmation(true);
 * } catch (error) {
 *   setError(error);
 * }
 * ```
 *
 * @see [Create your own UI for email auth](https://wasp.sh/docs/features/auth/email/create-your-own-ui)
 */
export declare function signup(
  data: { email: string; password: string } & UserEmailSignupFields,
): Promise<{ success: boolean }>;

/**
 * Signs the user up with
 * [Username & password](https://wasp.sh/docs/features/auth/username-and-pass/overview)
 * auth.
 *
 * Only available with Username & password auth. It doesn't log the user in:
 * call {@link login} after it.
 *
 * By default, Wasp only saves the `username` and `password` fields. If you
 * [define extra signup fields](https://wasp.sh/docs/features/auth/overview#customizing-the-signup-process),
 * pass them in `data` too.
 *
 * @param data The new user's data, including your extra signup fields.
 * @param data.username The user's username.
 * @param data.password The user's password.
 *
 * @example
 * ```ts
 * import { login, signup } from "wasp/client/auth";
 *
 * try {
 *   await signup({ username, password });
 *   await login({ username, password });
 *   navigate("/");
 * } catch (error) {
 *   setError(error);
 * }
 * ```
 *
 * @see [Create your own UI for username and password auth](https://wasp.sh/docs/features/auth/username-and-pass/create-your-own-ui)
 */
export declare function signup(
  data: {
    username: string;
    password: string;
  } & UserUsernameAndPasswordSignupFields,
): Promise<void>;

/**
 * Marks the user's email as verified and their account as active.
 *
 * Only available with [Email](https://wasp.sh/docs/features/auth/email/overview)
 * auth. Make sure to redirect the user on success (e.g. to the login page).
 *
 * @param data The verification data.
 * @param data.token The token Wasp created when the user signed up. Wasp puts
 * it in the `token` query parameter of the link in the verification email.
 *
 * @example
 * ```ts
 * import { verifyEmail } from "wasp/client/auth";
 *
 * const token = new URLSearchParams(location.search).get("token");
 * await verifyEmail({ token });
 * navigate("/login");
 * ```
 *
 * @see [Create your own UI for email auth](https://wasp.sh/docs/features/auth/email/create-your-own-ui)
 */
export declare function verifyEmail(data: {
  token: string;
}): Promise<{ success: boolean; reason?: string }>;

/**
 * Sends the user a password reset email. It doesn't reset their password.
 *
 * Only available with [Email](https://wasp.sh/docs/features/auth/email/overview)
 * auth.
 *
 * @param data The password reset request.
 * @param data.email The email of the user whose password to reset.
 *
 * @example
 * ```ts
 * import { requestPasswordReset } from "wasp/client/auth";
 *
 * await requestPasswordReset({ email: email.trim() });
 * ```
 *
 * @see [Create your own UI for email auth](https://wasp.sh/docs/features/auth/email/create-your-own-ui)
 */
export declare function requestPasswordReset(data: {
  email: string;
}): Promise<{ success: boolean }>;

/**
 * Confirms a password reset and sets the user's new password.
 *
 * Only available with [Email](https://wasp.sh/docs/features/auth/email/overview)
 * auth. Make sure to redirect the user on success (e.g. to the login page).
 *
 * @param data The password reset data.
 * @param data.token The token Wasp created when the user requested the
 * password reset. Wasp puts it in the `token` query parameter of the link in
 * the password reset email.
 * @param data.password The user's new password.
 *
 * @example
 * ```ts
 * import { resetPassword } from "wasp/client/auth";
 *
 * const token = new URLSearchParams(location.search).get("token");
 * await resetPassword({ token, password });
 * navigate("/login");
 * ```
 *
 * @see [Create your own UI for email auth](https://wasp.sh/docs/features/auth/email/create-your-own-ui)
 */
export declare function resetPassword(data: {
  token: string;
  password: string;
}): Promise<{ success: boolean }>;

// Social auth

/**
 * The URL that starts the [Google](https://wasp.sh/docs/features/auth/social-auth/google)
 * login flow. Use it as a link's `href` to build your own sign-in button.
 *
 * Only available with Google auth.
 *
 * @example
 * ```tsx
 * import { googleSignInUrl } from "wasp/client/auth";
 *
 * export const LoginPage = () => {
 *   return <a href={googleSignInUrl}>Sign in with Google</a>;
 * };
 * ```
 *
 * @see [Create your own UI for social auth](https://wasp.sh/docs/features/auth/social-auth/create-your-own-ui)
 */
export declare const googleSignInUrl: string;

/**
 * A button that starts the [Google](https://wasp.sh/docs/features/auth/social-auth/google)
 * login flow, styled like the Auth UI's.
 *
 * Only available with Google auth.
 *
 * @example
 * ```tsx
 * import { GoogleSignInButton } from "wasp/client/auth";
 *
 * export const LoginPage = () => {
 *   return <GoogleSignInButton />;
 * };
 * ```
 *
 * @see [Create your own UI for social auth](https://wasp.sh/docs/features/auth/social-auth/create-your-own-ui)
 */
export declare function GoogleSignInButton(): JSX.Element;

/**
 * The URL that starts the [GitHub](https://wasp.sh/docs/features/auth/social-auth/github)
 * login flow. Use it as a link's `href` to build your own sign-in button.
 *
 * Only available with GitHub auth.
 *
 * @example
 * ```tsx
 * import { githubSignInUrl } from "wasp/client/auth";
 *
 * export const LoginPage = () => {
 *   return <a href={githubSignInUrl}>Sign in with GitHub</a>;
 * };
 * ```
 *
 * @see [Create your own UI for social auth](https://wasp.sh/docs/features/auth/social-auth/create-your-own-ui)
 */
export declare const githubSignInUrl: string;

/**
 * A button that starts the [GitHub](https://wasp.sh/docs/features/auth/social-auth/github)
 * login flow, styled like the Auth UI's.
 *
 * Only available with GitHub auth.
 *
 * @example
 * ```tsx
 * import { GitHubSignInButton } from "wasp/client/auth";
 *
 * export const LoginPage = () => {
 *   return <GitHubSignInButton />;
 * };
 * ```
 *
 * @see [Create your own UI for social auth](https://wasp.sh/docs/features/auth/social-auth/create-your-own-ui)
 */
export declare function GitHubSignInButton(): JSX.Element;

/**
 * The URL that starts the [Discord](https://wasp.sh/docs/features/auth/social-auth/discord)
 * login flow. Use it as a link's `href` to build your own sign-in button.
 *
 * Only available with Discord auth.
 *
 * @see [Create your own UI for social auth](https://wasp.sh/docs/features/auth/social-auth/create-your-own-ui)
 */
export declare const discordSignInUrl: string;

/**
 * A button that starts the [Discord](https://wasp.sh/docs/features/auth/social-auth/discord)
 * login flow, styled like the Auth UI's.
 *
 * Only available with Discord auth.
 *
 * @see [Create your own UI for social auth](https://wasp.sh/docs/features/auth/social-auth/create-your-own-ui)
 */
export declare function DiscordSignInButton(): JSX.Element;

/**
 * The URL that starts the [Slack](https://wasp.sh/docs/features/auth/social-auth/slack)
 * login flow. Use it as a link's `href` to build your own sign-in button.
 *
 * Only available with Slack auth.
 *
 * @see [Create your own UI for social auth](https://wasp.sh/docs/features/auth/social-auth/create-your-own-ui)
 */
export declare const slackSignInUrl: string;

/**
 * A button that starts the [Slack](https://wasp.sh/docs/features/auth/social-auth/slack)
 * login flow, styled like the Auth UI's.
 *
 * Only available with Slack auth.
 *
 * @see [Create your own UI for social auth](https://wasp.sh/docs/features/auth/social-auth/create-your-own-ui)
 */
export declare function SlackSignInButton(): JSX.Element;

/**
 * The URL that starts the [Keycloak](https://wasp.sh/docs/features/auth/social-auth/keycloak)
 * login flow. Use it as a link's `href` to build your own sign-in button.
 *
 * Only available with Keycloak auth.
 *
 * @see [Create your own UI for social auth](https://wasp.sh/docs/features/auth/social-auth/create-your-own-ui)
 */
export declare const keycloakSignInUrl: string;

/**
 * A button that starts the [Keycloak](https://wasp.sh/docs/features/auth/social-auth/keycloak)
 * login flow, styled like the Auth UI's.
 *
 * Only available with Keycloak auth.
 *
 * @see [Create your own UI for social auth](https://wasp.sh/docs/features/auth/social-auth/create-your-own-ui)
 */
export declare function KeycloakSignInButton(): JSX.Element;

/**
 * The URL that starts the [Microsoft](https://wasp.sh/docs/features/auth/social-auth/microsoft)
 * login flow. Use it as a link's `href` to build your own sign-in button.
 *
 * Only available with Microsoft auth.
 *
 * @see [Create your own UI for social auth](https://wasp.sh/docs/features/auth/social-auth/create-your-own-ui)
 */
export declare const microsoftSignInUrl: string;

/**
 * A button that starts the [Microsoft](https://wasp.sh/docs/features/auth/social-auth/microsoft)
 * login flow, styled like the Auth UI's.
 *
 * Only available with Microsoft auth.
 *
 * @see [Create your own UI for social auth](https://wasp.sh/docs/features/auth/social-auth/create-your-own-ui)
 */
export declare function MicrosoftSignInButton(): JSX.Element;

export {};
