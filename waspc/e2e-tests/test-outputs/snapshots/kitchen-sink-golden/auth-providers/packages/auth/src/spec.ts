/**
 * The spec helper: what an app's `main.wasp.ts` imports.
 *
 * This module deliberately imports NOTHING -- not even `@wasp.sh/spec`. The
 * app compiles `main.wasp.ts` against its own copy of `@wasp.sh/spec`, and a
 * type that mentioned this package's copy would never be assignable to it
 * (the spec's branded types are unique per copy). So the manifest is
 * constructed and typed structurally here, and the compiler validates it
 * structurally when it reads the app.
 */

export const PROVIDER_ID = "wasp";
export const ROUTES_BASE_PATH = "/auth/wasp";
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
 * The configuration accepted by {@link waspAuth}. `Ref` is the app's own
 * `Reference` type (from `with { type: "ref" }` imports); it stays generic so
 * the app's branded references flow through untouched.
 */
export interface WaspAuthConfig<Ref = unknown> {
  /** Enabled authentication methods. At least one must be enabled. */
  methods: WaspAuthMethods<Ref>;
  /**
   * Route that Wasp redirects users to after a successful login or signup.
   * Only takes effect when using the built-in forms.
   * @default "/"
   */
  onAuthSucceededRedirectTo?: string;
  /** Called once, after the user verifies their email. Receives `email` and `user`. */
  onAfterEmailVerified?: Ref;
  /**
   * Called before redirecting the user to the OAuth provider. Receives the
   * generated `url` and `oauth.uniqueRequestId`. Return `{ url }` to override
   * the redirect URL.
   */
  onBeforeOAuthRedirect?: Ref;
}

export type WaspAuthMethods<Ref = unknown> = {
  usernameAndPassword?: UsernameAndPasswordConfig<Ref>;
  email?: EmailAuthConfig<Ref>;
} & Partial<Record<OAuthMethodName, SocialAuthConfig<Ref>>>;

export interface UsernameAndPasswordConfig<Ref = unknown> {
  /** Extra fields to save on the user during signup; see `defineUserSignupFields`. */
  userSignupFields?: Ref;
}

export interface SocialAuthConfig<Ref = unknown> {
  /** Extra fields to save on the user during signup, from the provider's profile. */
  userSignupFields?: Ref;
  /** Function returning the OAuth config (scopes, extra params) for this provider. */
  configFn?: Ref;
}

export interface EmailAuthConfig<Ref = unknown> {
  userSignupFields?: Ref;
  /** The sender of the verification and password reset emails. */
  fromField: { name?: string; email: string };
  emailVerification: EmailFlowConfig<Ref>;
  passwordReset: EmailFlowConfig<Ref>;
}

export interface EmailFlowConfig<Ref = unknown> {
  /** Path of the client route the emailed link points at (e.g. `"/email-verification"`). */
  clientRoute: string;
  /** Function returning the email content (subject, html, text) for this flow. */
  getEmailContentFn?: Ref;
}

type EnvVarRequirement = {
  name: string;
  optional?: boolean;
  doc?: string;
  devDefault?: string;
};

/**
 * The manifest {@link waspAuth} produces, structurally matching
 * `AuthProviderManifest` from `@wasp.sh/spec`.
 */
export type WaspAuthProviderManifest<Ref = unknown> = {
  readonly __waspAuthProviderManifest: true;
  kind: "external";
  contractVersion: 1;
  id: typeof PROVIDER_ID;
  server: { package: string };
  client: { package: string };
  routes: { basePath: typeof ROUTES_BASE_PATH };
  capabilities: string[];
  env: { server: EnvVarRequirement[]; client: EnvVarRequirement[] };
  uses: Array<"wasp-sessions" | "identity-namespaces" | "email-send">;
  identityNamespaces: string[];
  options: WaspAuthOptions;
  extensions: Record<string, Ref>;
};

/** The serializable options the server and client adapters are instantiated with. */
export type WaspAuthOptions = {
  onAuthSucceededRedirectTo: string;
  clientOAuthCallbackPath: string;
  routesBasePath: string;
  methods: {
    usernameAndPassword?: Record<string, never>;
    email?: {
      fromField: { name?: string; email: string };
      emailVerificationClientRoute: string;
      passwordResetClientRoute: string;
    };
  } & Partial<Record<OAuthProviderName, { requiredScopes: string[] }>>;
};

export type OAuthProviderName =
  | "google"
  | "github"
  | "keycloak"
  | "slack"
  | "discord"
  | "microsoft";

const oauthProviders: Record<
  OAuthMethodName,
  { name: OAuthProviderName; requiredScopes: string[]; envVars: string[] }
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
 * Declares Wasp's own auth as one of the app's auth providers.
 *
 * ```ts
 * import { waspAuth } from "@wasp.sh/auth/spec";
 *
 * auth: {
 *   userEntity: "User",
 *   onAuthFailedRedirectTo: "/login",
 *   providers: [waspAuth({ methods: { usernameAndPassword: {} } })],
 * }
 * ```
 *
 * The manifest mounts the flows at `/auth/wasp`, records identities under
 * `wasp:<method>`, declares the env vars the enabled methods read
 * (`JWT_SECRET` for email and OAuth, the OAuth client credentials), and hands
 * every user function over as an extension.
 */
export function waspAuth<Ref = unknown>(
  config: WaspAuthConfig<Ref>,
): WaspAuthProviderManifest<Ref> {
  const { methods } = config;
  const enabledOAuth = (
    Object.keys(oauthProviders) as OAuthMethodName[]
  ).filter((name) => methods[name] !== undefined);
  const usesEmail = methods.email !== undefined;
  const needsJwt = usesEmail || enabledOAuth.length > 0;

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

  const extensions: Record<string, Ref> = {};
  const addExtension = (name: string, ref: Ref | undefined) => {
    if (ref !== undefined) extensions[name] = ref;
  };
  addExtension(
    "usernameUserSignupFields",
    methods.usernameAndPassword?.userSignupFields,
  );
  addExtension("emailUserSignupFields", methods.email?.userSignupFields);
  addExtension(
    "getVerificationEmailContent",
    methods.email?.emailVerification.getEmailContentFn,
  );
  addExtension(
    "getPasswordResetEmailContent",
    methods.email?.passwordReset.getEmailContentFn,
  );
  addExtension("onAfterEmailVerified", config.onAfterEmailVerified);
  addExtension("onBeforeOAuthRedirect", config.onBeforeOAuthRedirect);
  for (const method of enabledOAuth) {
    const { name } = oauthProviders[method];
    addExtension(`${name}UserSignupFields`, methods[method]?.userSignupFields);
    addExtension(`${name}ConfigFn`, methods[method]?.configFn);
  }

  const optionMethods: WaspAuthOptions["methods"] = {};
  if (methods.usernameAndPassword !== undefined) {
    optionMethods.usernameAndPassword = {};
  }
  if (methods.email !== undefined) {
    optionMethods.email = {
      fromField: methods.email.fromField,
      emailVerificationClientRoute: methods.email.emailVerification.clientRoute,
      passwordResetClientRoute: methods.email.passwordReset.clientRoute,
    };
  }
  for (const method of enabledOAuth) {
    const { name, requiredScopes } = oauthProviders[method];
    optionMethods[name] = { requiredScopes };
  }

  const identityNamespaces = [
    PROVIDER_ID,
    ...(methods.usernameAndPassword !== undefined
      ? [`${PROVIDER_ID}:username`]
      : []),
    ...(usesEmail ? [`${PROVIDER_ID}:email`] : []),
    ...enabledOAuth.map(
      (method) => `${PROVIDER_ID}:${oauthProviders[method].name}`,
    ),
  ];

  return {
    __waspAuthProviderManifest: true,
    kind: "external",
    contractVersion: 1,
    id: PROVIDER_ID,
    server: { package: "@wasp.sh/auth/server" },
    client: { package: "@wasp.sh/auth/client" },
    routes: { basePath: ROUTES_BASE_PATH },
    capabilities: [],
    env: {
      server: [
        ...(needsJwt
          ? [
              {
                name: "JWT_SECRET",
                doc: "Signs email and OAuth tokens. openssl rand -base64 32",
                devDefault: "DEVJWTSECRET",
              },
            ]
          : []),
        ...(usesEmail
          ? [
              {
                name: "SKIP_EMAIL_VERIFICATION_IN_DEV",
                optional: true,
                doc: "Set to 'true' to skip email verification in development",
              },
            ]
          : []),
        ...enabledOAuth.flatMap((method) =>
          oauthProviders[method].envVars.map((name) => ({ name })),
        ),
      ],
      client: [],
    },
    uses: [
      "wasp-sessions",
      "identity-namespaces",
      ...(usesEmail ? (["email-send"] as const) : []),
    ],
    identityNamespaces,
    options: {
      onAuthSucceededRedirectTo: config.onAuthSucceededRedirectTo ?? "/",
      clientOAuthCallbackPath: OAUTH_CALLBACK_PATH,
      routesBasePath: ROUTES_BASE_PATH,
      methods: optionMethods,
    },
    extensions,
  };
}
