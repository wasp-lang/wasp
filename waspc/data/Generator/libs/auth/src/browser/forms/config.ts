import type { EmailAuthActions } from "../actions/email.js";
import type { UsernameAuthActions } from "../actions/username.js";

export type SocialProviderName =
  | "slack"
  | "discord"
  | "google"
  | "keycloak"
  | "github"
  | "microsoft";

export type SocialProvider = {
  name: SocialProviderName;
  /** The server URL that starts the provider's login flow. */
  signInUrl: string;
};

/** What the app decides about its auth forms. */
export type AuthFormsConfig = {
  /** Where the forms navigate after a successful login or signup. */
  onAuthSucceededRedirectTo: string;
  /** The email auth actions, when the app has email auth enabled. */
  email?: EmailAuthActions<object>;
  /** The username and password auth actions, when the app has that method enabled. */
  usernameAndPassword?: UsernameAuthActions<object>;
  /** The enabled OAuth providers, in the order their buttons are shown. */
  socialProviders: SocialProvider[];
  /** Whether to tell developers the Dummy email sender prints emails to the terminal. */
  isDummyEmailSenderNoticeEnabled: boolean;
};
