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
export declare const PROVIDER_ID = "wasp";
export declare const ROUTES_BASE_PATH = "/auth/wasp";
/** The client route the OAuth handback lands on; the app declares it. */
export declare const OAUTH_CALLBACK_PATH = "/oauth/callback";
export type OAuthMethodName = "google" | "gitHub" | "keycloak" | "slack" | "discord" | "microsoft";
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
    fromField: {
        name?: string;
        email: string;
    };
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
    server: {
        package: string;
    };
    client: {
        package: string;
    };
    routes: {
        basePath: typeof ROUTES_BASE_PATH;
    };
    capabilities: string[];
    env: {
        server: EnvVarRequirement[];
        client: EnvVarRequirement[];
    };
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
            fromField: {
                name?: string;
                email: string;
            };
            emailVerificationClientRoute: string;
            passwordResetClientRoute: string;
        };
    } & Partial<Record<OAuthProviderName, {
        requiredScopes: string[];
    }>>;
};
export type OAuthProviderName = "google" | "github" | "keycloak" | "slack" | "discord" | "microsoft";
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
export declare function waspAuth<Ref = unknown>(config: WaspAuthConfig<Ref>): WaspAuthProviderManifest<Ref>;
export {};
