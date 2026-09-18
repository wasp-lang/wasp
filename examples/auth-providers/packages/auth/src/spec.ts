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
export interface WaspAuthConfig<Ref = unknown, StoreRef = never> {
  /** Enabled authentication methods. At least one must be enabled. */
  methods: WaspAuthMethods<Ref>;
  /**
   * How a verified login turns into the credential the client carries
   * afterwards. By default the scheme runs its own issuer: a bearer token
   * whose record lives in the database (`{ transport: "bearer", store:
   * "prisma" }`). Pick `"cookie"` for an HttpOnly cookie, `"signed-token"`
   * for a self-contained credential without a table, or `{ scheme }` to sign
   * into a sibling scheme (a standalone `waspBearer()` / `waspCookie()`) that
   * several schemes share.
   */
  credentials?: WaspAuthCredentialsConfig<StoreRef>;
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

/**
 * `StoreRef` is the app's reference to its own credential store, kept
 * separate from the function references so the two never unify into one
 * (over-wide) type.
 */
export type WaspAuthCredentialsConfig<StoreRef = never> =
  | { scheme: string }
  | {
      transport?: "bearer" | "cookie";
      store?: "prisma" | "signed-token" | StoreRef;
      /** Credential lifetime, e.g. `"30d"` or `"15m"`. Default: 30 days. */
      ttl?: string;
    };

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
 * `AuthSchemeManifest` from `@wasp.sh/spec`.
 */
export type WaspAuthSchemeManifest<Ref = unknown, StoreRef = never> = {
  readonly __waspAuthSchemeManifest: true;
  kind: "scheme";
  contractVersion: 3;
  handler: string;
  server: { package: string };
  client: { package: string };
  routes: Record<string, never>;
  capabilities: string[];
  env: { server: EnvVarRequirement[]; client: EnvVarRequirement[] };
  uses: Array<"identity-namespaces" | "email-send">;
  /** Namespace suffixes; the compiler prefixes them with the scheme name. */
  identityNamespaces: string[];
  credentials: WaspAuthCredentialsConfig<StoreRef>;
  options: WaspAuthOptions;
  extensions: Record<string, Ref>;
};

/** The serializable options the server and client auth handlers are instantiated with. */
export type WaspAuthOptions = {
  onAuthSucceededRedirectTo: string;
  clientOAuthCallbackPath: string;
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
 * under `<scheme>:<method>`, declares the env vars the enabled methods read
 * (`JWT_SECRET` for email and OAuth, the OAuth client credentials), hands
 * out credentials through its own bearer issuer unless `credentials` says
 * otherwise, and hands every user function over as an extension.
 */
export function waspAuth<Ref = unknown, StoreRef = never>(
  config: WaspAuthConfig<Ref, StoreRef>,
): WaspAuthSchemeManifest<Ref, StoreRef> {
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
    ...(methods.usernameAndPassword !== undefined ? ["username"] : []),
    ...(usesEmail ? ["email"] : []),
    ...enabledOAuth.map((method) => oauthProviders[method].name),
  ];

  return {
    __waspAuthSchemeManifest: true,
    kind: "scheme",
    contractVersion: 3,
    handler: "@wasp.sh/auth",
    server: { package: "@wasp.sh/auth/server" },
    client: { package: "@wasp.sh/auth/client" },
    routes: {},
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
      "identity-namespaces",
      ...(usesEmail ? (["email-send"] as const) : []),
    ],
    identityNamespaces,
    credentials: config.credentials ?? { transport: "bearer", store: "prisma" },
    options: {
      onAuthSucceededRedirectTo: config.onAuthSucceededRedirectTo ?? "/",
      clientOAuthCallbackPath: OAUTH_CALLBACK_PATH,
      methods: optionMethods,
    },
    extensions,
  };
}
