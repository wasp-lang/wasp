export type OAuthProviderName = "google" | "github" | "slack" | "discord" | "keycloak" | "microsoft";
/**
 * The manifest's `client.config`, as the client factory receives it. Public
 * by construction (it is bundled into the browser), so it carries only what
 * the forms and actions read: where to go after login, where the OAuth
 * handback lands, and which methods are on.
 */
export type WaspAuthClientConfig = {
    /** Route the built-in forms navigate to after a successful login or signup. */
    onAuthSucceededRedirectTo: string;
    /** Client route the OAuth handback redirects to with the one-time code. */
    clientOAuthCallbackPath: string;
    methods: Partial<Record<"usernameAndPassword" | "email" | OAuthProviderName, Record<string, never>>>;
};
