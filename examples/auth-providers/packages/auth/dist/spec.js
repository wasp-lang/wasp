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
 * under the provider name of their method (`email`, `google`), declares the env vars the enabled methods read
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
    // A field is only present when the app gave it, so the config stays
    // minimal and `undefined` never has to cross the compiler.
    const given = (fields) => Object.fromEntries(Object.entries(fields).filter(([, value]) => value !== undefined));
    const serverMethods = {};
    const clientMethods = {};
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
                getVerificationEmailContent: methods.email.emailVerification.getEmailContentFn,
                getPasswordResetEmailContent: methods.email.passwordReset.getEmailContentFn,
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
    const providerNames = [
        ...(methods.usernameAndPassword !== undefined
            ? ["username"]
            : []),
        ...(usesEmail ? ["email"] : []),
        ...enabledOAuth.map((method) => oauthProviders[method].name),
    ];
    return {
        __waspAuthSchemeManifest: true,
        kind: "scheme",
        contractVersion: 12,
        server: {
            authAdapter: { package: "@wasp.sh/auth/server" },
            env: [
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
        providerNames,
        credentials: config.credentials ?? { transport: "bearer", store: "prisma" },
    };
}
