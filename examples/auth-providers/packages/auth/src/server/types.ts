import type {
  AuthResponse,
  JsonValue,
  WaspServerRuntime,
} from "@wasp.sh/auth-contract";
import type { IncomingMessage, ServerResponse } from "node:http";

/**
 * The runtime window: the base runtime plus the credentials facet the
 * manifest's `credentials` config wires (the scheme's private issuer, or a
 * sibling scheme it signs into). `email-send` is requested only when the
 * email method is on, so `runtime.email` stays optional in the type.
 */
export type WaspAuthRuntime = WaspServerRuntime<never, true>;

/** The wire-level answer of a sign-in, replayed by the one-time code. */
export type SignInResponse = AuthResponse;

export type OAuthProviderName =
  | "google"
  | "github"
  | "slack"
  | "discord"
  | "keycloak"
  | "microsoft";

export type MethodProviderName = "username" | "email" | OAuthProviderName;

/**
 * The manifest's `server.config`, as the server factory receives it: what
 * `waspAuth({ ... })` captured in `main.wasp.ts`, one object mixing plain
 * data with the app's functions, each next to the method it belongs to. Wasp
 * carried the functions across the compiler as references and set them back,
 * so they arrive live.
 */
export type WaspAuthServerConfig = {
  /** Client route the OAuth handback redirects to with the one-time code. */
  clientOAuthCallbackPath: string;
  methods: {
    usernameAndPassword?: { userSignupFields?: UserSignupFields };
    email?: {
      fromField: { name?: string; email: string };
      /** Client route path the emailed verification link points at. */
      emailVerificationClientRoute: string;
      /** Client route path the emailed password-reset link points at. */
      passwordResetClientRoute: string;
      userSignupFields?: UserSignupFields;
      getVerificationEmailContent?: GetVerificationEmailContentFn;
      getPasswordResetEmailContent?: GetPasswordResetEmailContentFn;
    };
  } & Partial<Record<OAuthProviderName, OAuthMethodServerConfig>>;
  onAfterEmailVerified?: OnAfterEmailVerifiedHook;
  onBeforeOAuthRedirect?: OnBeforeOAuthRedirectHook;
};

export type OAuthMethodServerConfig = {
  requiredScopes: string[];
  userSignupFields?: UserSignupFields;
  configFn?: () => Record<string, unknown>;
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
 * Use this type for typing your `onAfterEmailVerified` hook. Called exactly
 * once, after a user successfully verifies their email.
 */
export type OnAfterEmailVerifiedHook<
  User = unknown,
  Prisma = unknown,
> = (params: {
  /** The app's Prisma client. */
  prisma: Prisma;
  req: IncomingMessage;
  /** The email address that was verified. */
  email: string;
  /** The user who completed email verification. */
  user: User;
}) => void | Promise<void>;

/**
 * Use this type for typing your `onBeforeOAuthRedirect` hook.
 * @returns Object with a URL that the OAuth flow should redirect to.
 */
export type OnBeforeOAuthRedirectHook<Prisma = unknown> = (params: {
  /** The app's Prisma client. */
  prisma: Prisma;
  req: IncomingMessage;
  /** URL that the OAuth flow should redirect to. */
  url: URL;
  /** Unique request ID that was generated during the OAuth flow. */
  oauth: { uniqueRequestId: string };
}) => { url: URL } | Promise<{ url: URL }>;

export type OAuthData = {
  uniqueRequestId: string;
  providerName: OAuthProviderName;
  tokens: unknown;
};

/** What every route handler in this package receives. */
export type Ctx = {
  runtime: WaspAuthRuntime;
  config: WaspAuthServerConfig;
};

export type Req = IncomingMessage & {
  body?: unknown;
  url?: string;
  method?: string;
};
export type Res = ServerResponse;

export type Json = Record<string, JsonValue>;
