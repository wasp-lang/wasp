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
export declare const WASP_AUTH_PROVIDER_ID = "external:wasp-auth";
export type EnvVarRequirement = {
    name: string;
    optional?: boolean;
    doc?: string;
};
/**
 * The serializable options `waspAuthLib` captures in `main.wasp.ts`. Wasp
 * delivers them verbatim to BOTH the server and the client adapter factory,
 * so which methods exist is declared exactly once.
 */
export type WaspAuthLibOptions = {
    methods: {
        usernameAndPassword?: Record<string, never>;
        email?: {
            /** Client route the emailed verification link points at. */
            emailVerificationPath: string;
            /** Client route the emailed password-reset link points at. */
            passwordResetPath: string;
            /** Sender identity; falls back to the app's `emailSender.defaultFrom`. */
            fromField?: {
                name?: string;
                email: string;
            };
            /** Log in unverified users while developing. Ignored in production. */
            skipEmailVerificationInDev?: boolean;
        };
        google?: Record<string, never>;
    };
    /** Client route that redeems the OAuth one-time code. */
    oauthCallbackPath?: string;
};
export type WaspAuthLibProviderManifest = {
    readonly __waspAuthProviderManifest: true;
    kind: "external";
    contractVersion: 1;
    id: typeof WASP_AUTH_PROVIDER_ID;
    server: {
        package: string;
    };
    client: {
        package: string;
    };
    routes: {
        basePath: `/${string}`;
    };
    capabilities: string[];
    env: {
        server: EnvVarRequirement[];
        client: EnvVarRequirement[];
    };
    uses: ("wasp-sessions" | "email-send" | "identity-namespaces")[];
    identityNamespaces: string[];
    options: WaspAuthLibOptions;
};
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
export declare function waspAuthLib(options: WaspAuthLibOptions): WaspAuthLibProviderManifest;
