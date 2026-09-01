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
export const WASP_AUTH_PROVIDER_ID = "external:wasp-auth";
/**
 * Declares Wasp's own auth -- externalized into this package -- as one of the
 * app's auth providers.
 *
 * ```ts
 * import { waspAuthLib } from "@wasp.sh/auth/spec";
 *
 * auth: {
 *   userEntity: "User",
 *   onAuthFailedRedirectTo: "/login",
 *   providers: [
 *     waspAuthLib({
 *       methods: {
 *         usernameAndPassword: {},
 *         email: {
 *           emailVerificationPath: "/email-verified",
 *           passwordResetPath: "/password-reset",
 *         },
 *         google: {},
 *       },
 *     }),
 *   ],
 * }
 * ```
 *
 * Every power the in-tree implementation has arrives through the contract:
 * `wasp-sessions` (its routes mint and revoke Wasp sessions),
 * `identity-namespaces` (each method records under its own namespace, e.g.
 * `external:wasp-auth/email`), and -- when the email method is enabled --
 * `email-send` (verification and reset mail through the app's emailSender;
 * requesting it without an `app.emailSender` is a compile error).
 */
export function waspAuthLib(options) {
    const { methods } = options;
    const enabledMethodNames = ["usernameAndPassword", "email", "google"].filter((name) => methods[name] !== undefined);
    if (enabledMethodNames.length === 0) {
        throw new Error("waspAuthLib requires at least one enabled method.");
    }
    return {
        __waspAuthProviderManifest: true,
        kind: "external",
        contractVersion: 1,
        id: WASP_AUTH_PROVIDER_ID,
        server: { package: "@wasp.sh/auth/server" },
        client: { package: "@wasp.sh/auth/client" },
        routes: { basePath: "/wasp-auth" },
        // A stateless verifier: Wasp's session is the only session there is.
        capabilities: [],
        env: {
            server: [
                {
                    name: "WASP_AUTH_TOKENS_SECRET",
                    devDefault: "DEV_WASP_AUTH_TOKENS_SECRET",
                    doc: "Signs email verification links, password reset links and OAuth one-time codes. Required in production, defaulted in development.",
                },
                ...(methods.google !== undefined
                    ? [
                        {
                            name: "WASP_AUTH_GOOGLE_CLIENT_ID",
                            doc: "Google OAuth client id (framework names like GOOGLE_CLIENT_ID are reserved for Wasp itself).",
                        },
                        {
                            name: "WASP_AUTH_GOOGLE_CLIENT_SECRET",
                            doc: "Google OAuth client secret.",
                        },
                    ]
                    : []),
            ],
            client: [],
        },
        uses: [
            "wasp-sessions",
            "identity-namespaces",
            ...(methods.email !== undefined ? ["email-send"] : []),
        ],
        identityNamespaces: [
            WASP_AUTH_PROVIDER_ID,
            ...enabledMethodNames.map((name) => `${WASP_AUTH_PROVIDER_ID}/${methodNamespaceSuffix[name]}`),
        ],
        options,
    };
}
const methodNamespaceSuffix = {
    usernameAndPassword: "username",
    email: "email",
    google: "google",
};
