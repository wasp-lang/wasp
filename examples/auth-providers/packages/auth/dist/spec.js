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
const oauthProviders = {
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
export function waspAuth(config) {
    const { methods } = config;
    const enabledOAuth = Object.keys(oauthProviders).filter((name) => methods[name] !== undefined);
    const usesEmail = methods.email !== undefined;
    const needsJwt = usesEmail || enabledOAuth.length > 0;
    if (methods.usernameAndPassword === undefined &&
        !usesEmail &&
        enabledOAuth.length === 0) {
        throw new Error("waspAuth(): at least one auth method must be enabled.");
    }
    if (methods.usernameAndPassword !== undefined && usesEmail) {
        throw new Error("waspAuth(): use either usernameAndPassword or email, not both.");
    }
    const extensions = {};
    const addExtension = (name, ref) => {
        if (ref !== undefined)
            extensions[name] = ref;
    };
    addExtension("usernameUserSignupFields", methods.usernameAndPassword?.userSignupFields);
    addExtension("emailUserSignupFields", methods.email?.userSignupFields);
    addExtension("getVerificationEmailContent", methods.email?.emailVerification.getEmailContentFn);
    addExtension("getPasswordResetEmailContent", methods.email?.passwordReset.getEmailContentFn);
    addExtension("onAfterEmailVerified", config.onAfterEmailVerified);
    addExtension("onBeforeOAuthRedirect", config.onBeforeOAuthRedirect);
    for (const method of enabledOAuth) {
        const { name } = oauthProviders[method];
        addExtension(`${name}UserSignupFields`, methods[method]?.userSignupFields);
        addExtension(`${name}ConfigFn`, methods[method]?.configFn);
    }
    const optionMethods = {};
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
        ...enabledOAuth.map((method) => `${PROVIDER_ID}:${oauthProviders[method].name}`),
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
                ...enabledOAuth.flatMap((method) => oauthProviders[method].envVars.map((name) => ({ name }))),
            ],
            client: [],
        },
        uses: [
            "wasp-sessions",
            "identity-namespaces",
            ...(usesEmail ? ["email-send"] : []),
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
