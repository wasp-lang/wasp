export type OAuthProviderName =
  | "google"
  | "github"
  | "slack"
  | "discord"
  | "keycloak"
  | "microsoft";

/** The same serializable options the server adapter receives. */
export type WaspAuthClientOptions = {
  onAuthSucceededRedirectTo: string;
  /** Where the server mounted the routes. Defaults to `/auth`. */
  routesBasePath?: string;
  clientOAuthCallbackPath: string;
  methods: {
    usernameAndPassword?: Record<string, never>;
    email?: {
      fromField: { name?: string; email: string };
      emailVerificationClientRoute: string;
      passwordResetClientRoute: string;
    };
  } & Partial<Record<OAuthProviderName, unknown>>;
};
