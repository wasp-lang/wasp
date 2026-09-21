import { jsx as _jsx } from "react/jsx-runtime";
import { signInUrl } from "./actions.js";
import { SocialButton } from "./forms/internal/social/SocialButton.js";
import * as SocialIcons from "./forms/internal/social/SocialIcons.js";
import { setClientState } from "./runtime.js";
/**
 * The client half of Wasp's own auth. Wasp instantiates it like any client
 * handler; the forms and actions below then read the captured runtime
 * (`mountUrl`, the scheme-bound `setCredential` sink) and options.
 */
export const createClientAuthHandler = (runtime, spec) => {
    setClientState(runtime, spec);
    // No Wrapper, no ambient credential: a bearer credential is adopted
    // explicitly by the login actions through the setCredential sink, and a
    // cookie one never touches the client at all.
    return {};
};
// PUBLIC API -- the surface `wasp/client/auth` re-exports.
export { confirmMerge, linkEmail, linkUsername, login, requestPasswordReset, resetPassword, signup, startOAuthLink, unlink, verifyEmail, } from "./actions.js";
export { ForgotPasswordForm, LoginForm, ResetPasswordForm, SignupForm, VerifyEmailForm, } from "./forms/index.js";
export { FormError, FormInput, FormItemGroup, FormLabel, FormTextarea, SubmitButton, } from "./forms/internal/Form.js";
export { OAuthCallbackPage } from "./OAuthCallbackPage.js";
// PUBLIC API -- per-provider sign-in URLs and buttons.
export const googleSignInUrl = () => signInUrl("google");
export const gitHubSignInUrl = () => signInUrl("github");
export const slackSignInUrl = () => signInUrl("slack");
export const discordSignInUrl = () => signInUrl("discord");
export const keycloakSignInUrl = () => signInUrl("keycloak");
export const microsoftSignInUrl = () => signInUrl("microsoft");
export function GoogleSignInButton() {
    return (_jsx(SocialButton, { href: signInUrl("google"), children: _jsx(SocialIcons.Google, {}) }));
}
export function GitHubSignInButton() {
    return (_jsx(SocialButton, { href: signInUrl("github"), children: _jsx(SocialIcons.GitHub, {}) }));
}
export function SlackSignInButton() {
    return (_jsx(SocialButton, { href: signInUrl("slack"), children: _jsx(SocialIcons.Slack, {}) }));
}
export function DiscordSignInButton() {
    return (_jsx(SocialButton, { href: signInUrl("discord"), children: _jsx(SocialIcons.Discord, {}) }));
}
export function KeycloakSignInButton() {
    return (_jsx(SocialButton, { href: signInUrl("keycloak"), children: _jsx(SocialIcons.Keycloak, {}) }));
}
export function MicrosoftSignInButton() {
    return (_jsx(SocialButton, { href: signInUrl("microsoft"), children: _jsx(SocialIcons.Microsoft, {}) }));
}
