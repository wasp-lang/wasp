/**
 * The spec constructor: what an app's `main.wasp.ts` imports.
 *
 * This module deliberately imports nothing at runtime, and no type from
 * `@wasp.sh/spec`. The app compiles `main.wasp.ts` against its own copy of
 * `@wasp.sh/spec`, and a type that mentioned this package's copy would never
 * be assignable to it (the spec's branded types are unique per copy). So the
 * manifest is constructed and typed structurally here, and the compiler
 * validates it structurally when it reads the app.
 *
 * A field that takes the app's code is a `SpecReference<AppValue>`: the app
 * passes a `with { type: "ref" }` import, and the server adapter receives
 * `AppValue` in that place. The adapters derive all their types from
 * `typeof waspAuth`, so the shape of the spec is written once, here.
 */

import type { CredentialStore, SpecReference } from "@wasp.sh/auth-contract";
import type { WaspAuthClientSpec } from "./client/types.js";
import type {
  GetPasswordResetEmailContentFn,
  GetVerificationEmailContentFn,
  OAuthConfigFn,
  OAuthProviderName,
  OnAfterEmailVerifiedHook,
  OnBeforeOAuthRedirectHook,
  UserSignupFields,
} from "./server/types.js";

export type { OAuthProviderName, WaspAuthClientSpec };

/** The client route the OAuth handback lands on; the app declares it. */
export const OAUTH_CALLBACK_PATH = "/oauth/callback";

export type OAuthMethodName =
  | "google"
  | "gitHub"
  | "keycloak"
  | "slack"
  | "discord"
  | "microsoft";

/**
 * The configuration accepted by {@link waspAuth}. Fields that take app code
 * expect a `with { type: "ref" }` import of it.
 */
export interface WaspAuthConfig {
  /** Enabled authentication methods. At least one must be enabled. */
  methods: WaspAuthMethods;
  /**
   * How a verified login turns into the credential the client carries
   * afterwards. By default the scheme runs its own issuer: a bearer token
   * whose record lives in the database (`{ transport: "bearer", store:
   * "prisma" }`). Pick `"cookie"` for an HttpOnly cookie, `"signed-token"`
   * for a self-contained credential without a table, or `{ scheme }` to sign
   * into a sibling scheme (a standalone `waspBearer()` / `waspCookie()`) that
   * several schemes share.
   */
  credentials?: WaspAuthCredentialsConfig;
  /**
   * Route that Wasp redirects users to after a successful login or signup.
   * Only takes effect when using the built-in forms.
   * @default "/"
   */
  onAuthSucceededRedirectTo?: string;
  /** Called once, after the user verifies their email. Receives `email` and `user`. */
  onAfterEmailVerified?: SpecReference<OnAfterEmailVerifiedHook>;
  /**
   * Called before redirecting the user to the OAuth provider. Receives the
   * generated `url` and `oauth.uniqueRequestId`. Return `{ url }` to override
   * the redirect URL.
   */
  onBeforeOAuthRedirect?: SpecReference<OnBeforeOAuthRedirectHook>;
}

export type WaspAuthCredentialsConfig =
  | { scheme: string }
  | {
      transport?: "bearer" | "cookie";
      /** `"prisma"` (default), `"signed-token"`, or the app's own `CredentialStore`. */
      store?: "prisma" | "signed-token" | SpecReference<CredentialStore>;
      /** Credential lifetime, e.g. `"30d"` or `"15m"`. Default: 30 days. */
      ttl?: string;
    };

export type WaspAuthMethods = {
  usernameAndPassword?: UsernameAndPasswordConfig;
  email?: EmailAuthConfig;
} & Partial<Record<OAuthMethodName, SocialAuthConfig>>;

export interface UsernameAndPasswordConfig {
  /** Extra fields to save on the user during signup; see `defineUserSignupFields`. */
  userSignupFields?: SpecReference<UserSignupFields>;
}

export interface SocialAuthConfig {
  /** Extra fields to save on the user during signup, from the provider's profile. */
  userSignupFields?: SpecReference<UserSignupFields>;
  /** Function returning the OAuth config (scopes, extra params) for this provider. */
  configFn?: SpecReference<OAuthConfigFn>;
}

export interface EmailAuthConfig {
  userSignupFields?: SpecReference<UserSignupFields>;
  /** The sender of the verification and password reset emails. */
  fromField: { name?: string; email: string };
  emailVerification: EmailFlowConfig<GetVerificationEmailContentFn>;
  passwordReset: EmailFlowConfig<GetPasswordResetEmailContentFn>;
}

export interface EmailFlowConfig<GetEmailContentFn> {
  /** Path of the client route the emailed link points at (e.g. `"/email-verification"`). */
  clientRoute: string;
  /** Function returning the email content (subject, html, text) for this flow. */
  getEmailContentFn?: SpecReference<GetEmailContentFn>;
}

type EnvVarRequirement = {
  name: string;
  optional?: boolean;
  doc?: string;
  devDefault?: string;
};

/**
 * The manifest {@link waspAuth} produces, structurally matching
 * `AuthSchemeManifest` from `@wasp.sh/spec`. It is grouped by side: `server`
 * and `client` each hold what that half of the handler receives.
 */
export type WaspAuthSchemeManifest = {
  readonly __waspAuthSchemeManifest: true;
  kind: "scheme";
  contractVersion: 14;
  server: {
    authAdapter: { package: string };
    /** Which of these an app declares depends on its enabled methods. */
    env: Array<EnvVarRequirement & { name: WaspAuthServerEnvVarName }>;
    spec: WaspAuthManifestServerSpec;
    routes: Record<string, never>;
  };
  client: {
    authAdapter: { package: string };
    spec: WaspAuthClientSpec;
  };
  capabilities: string[];
  uses: Array<"email-send">;
  /**
   * One provider per enabled method, under the method's name. The OAuth ones
   * declare their kind, so every signup, login and link through them must
   * hand the provider's tokens to Wasp.
   */
  providers: {
    [ProviderName in "username" | "email"]?: Record<string, never>;
  } & { [ProviderName in OAuthProviderName]?: { kind: "oauth" } };
  credentials: WaspAuthCredentialsConfig;
};

/**
 * The manifest's `server.spec`: plain data mixed with references to the
 * app's functions, each next to the method it belongs to. The server adapter
 * receives the same object with the functions live (`WaspAuthServerSpec`).
 */
export type WaspAuthManifestServerSpec = {
  /** Client route the OAuth handback redirects to with the one-time code. */
  clientOAuthCallbackPath: string;
  methods: {
    usernameAndPassword?: {
      userSignupFields?: SpecReference<UserSignupFields>;
    };
    email?: {
      fromField: { name?: string; email: string };
      /** Client route path the emailed verification link points at. */
      emailVerificationClientRoute: string;
      /** Client route path the emailed password-reset link points at. */
      passwordResetClientRoute: string;
      userSignupFields?: SpecReference<UserSignupFields>;
      getVerificationEmailContent?: SpecReference<GetVerificationEmailContentFn>;
      getPasswordResetEmailContent?: SpecReference<GetPasswordResetEmailContentFn>;
    };
  } & Partial<Record<OAuthProviderName, OAuthMethodManifestServerSpec>>;
  onAfterEmailVerified?: SpecReference<OnAfterEmailVerifiedHook>;
  onBeforeOAuthRedirect?: SpecReference<OnBeforeOAuthRedirectHook>;
};

export type OAuthMethodManifestServerSpec = {
  requiredScopes: string[];
  userSignupFields?: SpecReference<UserSignupFields>;
  configFn?: SpecReference<OAuthConfigFn>;
};

export type WaspAuthServerEnvVarName =
  | "JWT_SECRET"
  | "SKIP_EMAIL_VERIFICATION_IN_DEV"
  | "GOOGLE_CLIENT_ID"
  | "GOOGLE_CLIENT_SECRET"
  | "GITHUB_CLIENT_ID"
  | "GITHUB_CLIENT_SECRET"
  | "KEYCLOAK_CLIENT_ID"
  | "KEYCLOAK_CLIENT_SECRET"
  | "KEYCLOAK_REALM_URL"
  | "SLACK_CLIENT_ID"
  | "SLACK_CLIENT_SECRET"
  | "DISCORD_CLIENT_ID"
  | "DISCORD_CLIENT_SECRET"
  | "MICROSOFT_CLIENT_ID"
  | "MICROSOFT_CLIENT_SECRET"
  | "MICROSOFT_TENANT_ID";

const oauthProviders: Record<
  OAuthMethodName,
  {
    name: OAuthProviderName;
    requiredScopes: string[];
    envVars: WaspAuthServerEnvVarName[];
  }
> = {
  google: {
    name: "google",
    requiredScopes: ["profile"],
    envVars: ["GOOGLE_CLIENT_ID", "GOOGLE_CLIENT_SECRET"],
  },
  gitHub: {
    name: "github",
    requiredScopes: [],
    envVars: ["GITHUB_CLIENT_ID", "GITHUB_CLIENT_SECRET"],
  },
  keycloak: {
    name: "keycloak",
    requiredScopes: ["profile"],
    envVars: [
      "KEYCLOAK_CLIENT_ID",
      "KEYCLOAK_CLIENT_SECRET",
      "KEYCLOAK_REALM_URL",
    ],
  },
  slack: {
    name: "slack",
    requiredScopes: ["openid"],
    envVars: ["SLACK_CLIENT_ID", "SLACK_CLIENT_SECRET"],
  },
  discord: {
    name: "discord",
    requiredScopes: ["identify"],
    envVars: ["DISCORD_CLIENT_ID", "DISCORD_CLIENT_SECRET"],
  },
  microsoft: {
    name: "microsoft",
    requiredScopes: ["openid", "profile", "email"],
    envVars: [
      "MICROSOFT_CLIENT_ID",
      "MICROSOFT_CLIENT_SECRET",
      "MICROSOFT_TENANT_ID",
    ],
  },
};

/**
 * Declares Wasp's own auth as one of the app's auth schemes.
 *
 * ```ts
 * import { waspAuth } from "@wasp.sh/auth/spec";
 *
 * auth: {
 *   userEntity: "User",
 *   onAuthFailedRedirectTo: "/login",
 *   schemes: { wasp: waspAuth({ methods: { usernameAndPassword: {} } }) },
 * }
 * ```
 *
 * The manifest mounts the flows at `/auth/<scheme>`, records identities
 * under the provider name of their method (`email`, `google`), declares the env vars the enabled methods read
 * (`JWT_SECRET` for email and OAuth, the OAuth client credentials), hands
 * out credentials through its own bearer issuer unless `credentials` says
 * otherwise, and hands every user function over as an extension.
 */
export function waspAuth(config: WaspAuthConfig): WaspAuthSchemeManifest {
  const { methods } = config;
  const enabledOAuth = (
    Object.keys(oauthProviders) as OAuthMethodName[]
  ).filter((name) => methods[name] !== undefined);
  const usesEmail = methods.email !== undefined;
  // Email and OAuth tokens need it, and so does the account-merging ticket,
  // which any method can issue. Whether the app turns merging on
  // (`auth.mergeUsers`) is not visible from here, so it is always declared;
  // the dev default keeps development working without setting it.
  const needsJwt = true;

  if (
    methods.usernameAndPassword === undefined &&
    !usesEmail &&
    enabledOAuth.length === 0
  ) {
    throw new Error("waspAuth(): at least one auth method must be enabled.");
  }
  if (methods.usernameAndPassword !== undefined && usesEmail) {
    throw new Error(
      "waspAuth(): use either usernameAndPassword or email, not both.",
    );
  }

  // A field is only present when the app gave it, so the config stays
  // minimal and `undefined` never has to cross the compiler.
  const given = <T extends Record<string, unknown>>(fields: T): Partial<T> =>
    Object.fromEntries(
      Object.entries(fields).filter(([, value]) => value !== undefined),
    ) as Partial<T>;

  const serverMethods: WaspAuthManifestServerSpec["methods"] = {};
  const clientMethods: WaspAuthClientSpec["methods"] = {};
  if (methods.usernameAndPassword !== undefined) {
    serverMethods.usernameAndPassword = given({
      userSignupFields: methods.usernameAndPassword.userSignupFields,
    });
    clientMethods.usernameAndPassword = {};
  }
  if (methods.email !== undefined) {
    serverMethods.email = {
      fromField: methods.email.fromField,
      emailVerificationClientRoute: methods.email.emailVerification.clientRoute,
      passwordResetClientRoute: methods.email.passwordReset.clientRoute,
      ...given({
        userSignupFields: methods.email.userSignupFields,
        getVerificationEmailContent:
          methods.email.emailVerification.getEmailContentFn,
        getPasswordResetEmailContent:
          methods.email.passwordReset.getEmailContentFn,
      }),
    };
    clientMethods.email = {};
  }
  for (const method of enabledOAuth) {
    const { name, requiredScopes } = oauthProviders[method];
    serverMethods[name] = {
      requiredScopes,
      ...given({
        userSignupFields: methods[method]?.userSignupFields,
        configFn: methods[method]?.configFn,
      }),
    };
    clientMethods[name] = {};
  }

  const providers: WaspAuthSchemeManifest["providers"] = {};
  if (methods.usernameAndPassword !== undefined) {
    providers.username = {};
  }
  if (usesEmail) {
    providers.email = {};
  }
  for (const method of enabledOAuth) {
    providers[oauthProviders[method].name] = { kind: "oauth" };
  }

  return {
    __waspAuthSchemeManifest: true,
    kind: "scheme",
    contractVersion: 14,
    server: {
      authAdapter: { package: "@wasp.sh/auth/server" },
      env: [
        ...(needsJwt
          ? [
              {
                name: "JWT_SECRET" as const,
                doc: "Signs email and OAuth tokens. openssl rand -base64 32",
                devDefault: "DEVJWTSECRET",
              },
            ]
          : []),
        ...(usesEmail
          ? [
              {
                name: "SKIP_EMAIL_VERIFICATION_IN_DEV" as const,
                optional: true,
                doc: "Set to 'true' to skip email verification in development",
              },
            ]
          : []),
        ...enabledOAuth.flatMap((method) =>
          oauthProviders[method].envVars.map((name) => ({ name })),
        ),
      ],
      spec: {
        clientOAuthCallbackPath: OAUTH_CALLBACK_PATH,
        methods: serverMethods,
        ...given({
          onAfterEmailVerified: config.onAfterEmailVerified,
          onBeforeOAuthRedirect: config.onBeforeOAuthRedirect,
        }),
      },
      routes: {},
    },
    client: {
      authAdapter: { package: "@wasp.sh/auth/client" },
      spec: {
        onAuthSucceededRedirectTo: config.onAuthSucceededRedirectTo ?? "/",
        clientOAuthCallbackPath: OAUTH_CALLBACK_PATH,
        methods: clientMethods,
      },
    },
    capabilities: [],
    uses: usesEmail ? ["email-send"] : [],
    providers,
    credentials: config.credentials ?? { transport: "bearer", store: "prisma" },
  };
}
