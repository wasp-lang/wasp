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
export function waspAuth(config) {
    const { methods } = config;
    const enabledOAuth = Object.keys(oauthProviders).filter((name) => methods[name] !== undefined);
    const usesEmail = methods.email !== undefined;
    // Email and OAuth tokens need it, and so does the account-merging ticket,
    // which any method can issue. Whether the app turns merging on
    // (`auth.mergeUsers`) is not visible from here, so it is always declared;
    // the dev default keeps development working without setting it.
    const needsJwt = true;
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
        ...(methods.usernameAndPassword !== undefined ? ["username"] : []),
        ...(usesEmail ? ["email"] : []),
        ...enabledOAuth.map((method) => oauthProviders[method].name),
    ];
    return {
        __waspAuthSchemeManifest: true,
        kind: "scheme",
        contractVersion: 2,
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
                ...enabledOAuth.flatMap((method) => oauthProviders[method].envVars.map((name) => ({ name }))),
            ],
            client: [],
        },
        uses: [
            "identity-namespaces",
            ...(usesEmail ? ["email-send"] : []),
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
