import type { ClientAdapterFactory } from "@wasp.sh/auth-contract/client";

import { signInUrl } from "./actions.js";
import { SocialButton } from "./forms/internal/social/SocialButton.js";
import * as SocialIcons from "./forms/internal/social/SocialIcons.js";
import { setClientState } from "./runtime.js";
import type { WaspAuthClientOptions } from "./types.js";

/**
 * The client half of Wasp's own auth. Wasp instantiates it like any client
 * adapter; the forms and actions below then read the captured runtime
 * (`apiUrl`, the provider-bound `setSession` sink) and options.
 */
export const createClientAdapter: ClientAdapterFactory<
  WaspAuthClientOptions
> = (runtime, options) => {
  setClientState(runtime, options);
  // No Wrapper, no ambient credential: sessions are adopted explicitly by
  // the login actions through the setSession sink.
  return {};
};

// PUBLIC API -- the surface `wasp/client/auth` re-exports.
export {
  login,
  requestPasswordReset,
  resetPassword,
  signup,
  verifyEmail,
} from "./actions.js";
export {
  ForgotPasswordForm,
  LoginForm,
  ResetPasswordForm,
  SignupForm,
  VerifyEmailForm,
} from "./forms/index.js";
export {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
  FormTextarea,
  SubmitButton,
} from "./forms/internal/Form.js";
export type { CustomizationOptions } from "./forms/types.js";
export { OAuthCallbackPage } from "./OAuthCallbackPage.js";
export type { WaspAuthClientOptions } from "./types.js";

// PUBLIC API -- per-provider sign-in URLs and buttons.
export const googleSignInUrl = () => signInUrl("google");
export const gitHubSignInUrl = () => signInUrl("github");
export const slackSignInUrl = () => signInUrl("slack");
export const discordSignInUrl = () => signInUrl("discord");
export const keycloakSignInUrl = () => signInUrl("keycloak");
export const microsoftSignInUrl = () => signInUrl("microsoft");

export function GoogleSignInButton() {
  return (
    <SocialButton href={signInUrl("google")}>
      <SocialIcons.Google />
    </SocialButton>
  );
}
export function GitHubSignInButton() {
  return (
    <SocialButton href={signInUrl("github")}>
      <SocialIcons.GitHub />
    </SocialButton>
  );
}
export function SlackSignInButton() {
  return (
    <SocialButton href={signInUrl("slack")}>
      <SocialIcons.Slack />
    </SocialButton>
  );
}
export function DiscordSignInButton() {
  return (
    <SocialButton href={signInUrl("discord")}>
      <SocialIcons.Discord />
    </SocialButton>
  );
}
export function KeycloakSignInButton() {
  return (
    <SocialButton href={signInUrl("keycloak")}>
      <SocialIcons.Keycloak />
    </SocialButton>
  );
}
export function MicrosoftSignInButton() {
  return (
    <SocialButton href={signInUrl("microsoft")}>
      <SocialIcons.Microsoft />
    </SocialButton>
  );
}
