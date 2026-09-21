import type {
  AuthResponse,
  JsonValue,
  ServerSpecOf,
  WaspServerRuntimeFor,
} from "@wasp.sh/auth-contract";
import type { IncomingMessage, ServerResponse } from "node:http";
import type { waspAuth } from "../spec.js";

/**
 * The runtime window, typed from the manifest `waspAuth()` returns:
 * `identities` keyed by this handler's methods, `env` by the env vars it
 * declares, `credentialsIssuer` always there (the manifest always declares
 * `credentials`), and `email` reachable only after checking `runtime.canSendEmail` (the
 * manifest requests `email-send` only when the email method is on).
 */
export type WaspAuthRuntime = WaspServerRuntimeFor<typeof waspAuth>;

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
 * The manifest's `server.spec`, as the server adapter receives it: what
 * `waspAuth({ ... })` captured in `main.wasp.ts`, with the app's functions
 * live where the references were. Derived from the constructor, so it cannot
 * drift from what the constructor builds.
 */
export type WaspAuthServerSpec = ServerSpecOf<typeof waspAuth>;

export type OAuthConfigFn = () => Record<string, unknown>;

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
  spec: WaspAuthServerSpec;
};

export type Req = IncomingMessage & {
  body?: unknown;
  url?: string;
  method?: string;
};
export type Res = ServerResponse;

export type Json = Record<string, JsonValue>;
