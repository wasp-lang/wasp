import type { ClientAdapterFactory } from "@wasp.sh/auth-contract/client";
import type { WaspAuthClientOptions } from "./types.js";
/**
 * The client half of Wasp's own auth. Wasp instantiates it like any client
 * adapter; the forms and actions below then read the captured runtime
 * (`apiUrl`, the provider-bound `setSession` sink) and options.
 */
export declare const createClientAdapter: ClientAdapterFactory<WaspAuthClientOptions>;
export { login, requestPasswordReset, resetPassword, signup, verifyEmail, } from "./actions.js";
export { ForgotPasswordForm, LoginForm, ResetPasswordForm, SignupForm, VerifyEmailForm, } from "./forms/index.js";
export { FormError, FormInput, FormItemGroup, FormLabel, FormTextarea, SubmitButton, } from "./forms/internal/Form.js";
export type { CustomizationOptions } from "./forms/types.js";
export { OAuthCallbackPage } from "./OAuthCallbackPage.js";
export type { WaspAuthClientOptions } from "./types.js";
export declare const googleSignInUrl: () => string;
export declare const gitHubSignInUrl: () => string;
export declare const slackSignInUrl: () => string;
export declare const discordSignInUrl: () => string;
export declare const keycloakSignInUrl: () => string;
export declare const microsoftSignInUrl: () => string;
export declare function GoogleSignInButton(): import("react").JSX.Element;
export declare function GitHubSignInButton(): import("react").JSX.Element;
export declare function SlackSignInButton(): import("react").JSX.Element;
export declare function DiscordSignInButton(): import("react").JSX.Element;
export declare function KeycloakSignInButton(): import("react").JSX.Element;
export declare function MicrosoftSignInButton(): import("react").JSX.Element;
