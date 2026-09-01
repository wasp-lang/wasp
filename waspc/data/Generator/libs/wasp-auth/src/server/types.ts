import type { JsonValue, WaspServerRuntime } from "@wasp.sh/auth-contract";
import type { IncomingMessage, ServerResponse } from "node:http";

/** The runtime grants Wasp's own auth runs on. */
export type WaspAuthGrants = "wasp-sessions" | "identity-namespaces";
export type WaspAuthRuntime = WaspServerRuntime<WaspAuthGrants>;

export type OAuthProviderName =
  | "google"
  | "github"
  | "slack"
  | "discord"
  | "keycloak"
  | "microsoft";

export type MethodProviderName = "username" | "email" | OAuthProviderName;

/**
 * The serializable options the generator derives from `waspAuth({ ... })`
 * in `main.wasp.ts`. Everything user-code shaped travels separately, as
 * {@link WaspAuthExtensions}.
 */
export type WaspAuthOptions = {
  onAuthSucceededRedirectTo: string;
  /** Client route the OAuth handback redirects to with the one-time code. */
  clientOAuthCallbackPath: string;
  methods: {
    usernameAndPassword?: Record<string, never>;
    email?: {
      fromField: { name?: string; email: string };
      /** Client route path the emailed verification link points at. */
      emailVerificationClientRoute: string;
      /** Client route path the emailed password-reset link points at. */
      passwordResetClientRoute: string;
    };
  } & Partial<Record<OAuthProviderName, { requiredScopes: string[] }>>;
};

export type UserSignupFields = Record<
  string,
  (data: Record<string, unknown>) => unknown
>;

export type EmailContent = { subject: string; html: string; text: string };
export type GetVerificationEmailContentFn = (params: {
  verificationLink: string;
}) => EmailContent;
export type GetPasswordResetEmailContentFn = (params: {
  passwordResetLink: string;
}) => EmailContent;

/**
 * The user-code pieces of Wasp's auth config, delivered by the generator
 * through virtual user modules: per-method `userSignupFields`, OAuth
 * `configFn`s, email content functions, and the method-specific hooks.
 */
export type WaspAuthExtensions = {
  userSignupFields?: Partial<Record<MethodProviderName, UserSignupFields>>;
  configFns?: Partial<Record<OAuthProviderName, () => Record<string, unknown>>>;
  getVerificationEmailContent?: GetVerificationEmailContentFn;
  getPasswordResetEmailContent?: GetPasswordResetEmailContentFn;
  onAfterEmailVerified?: (params: Record<string, unknown>) => unknown;
  onBeforeOAuthRedirect?: (params: Record<string, unknown>) => unknown;
};

export type OAuthData = {
  uniqueRequestId: string;
  providerName: OAuthProviderName;
  tokens: unknown;
};

/** What every route handler in this package receives. */
export type Ctx = {
  runtime: WaspAuthRuntime;
  options: WaspAuthOptions;
  extensions: WaspAuthExtensions;
};

export type Req = IncomingMessage & {
  body?: unknown;
  url?: string;
  method?: string;
};
export type Res = ServerResponse;

export type Json = Record<string, JsonValue>;
