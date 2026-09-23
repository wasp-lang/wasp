/**
 * The contract between Wasp and an auth handler.
 *
 * Wasp builds everything users experience on top of this interface -- `authRequired`
 * pages, `auth: true` operations, `context.user`, `useAuth()` -- so implementing it
 * is all it takes to make any auth solution (Wasp's own auth, Better Auth, Clerk,
 * ...) a Wasp auth scheme.
 *
 * Vocabulary, borrowed from ASP.NET:
 *
 * - A **handler** is the code: an `CredentialHandler` object. An handler package
 *   implements one in its server entry and exposes it as a named
 *   `createServerAuthHandler` export (see `ServerAuthAdapter`).
 * - A **scheme** is a named, configured instance of a handler, declared in the
 *   app's `auth.schemes` map. The scheme name is what Wasp records everywhere:
 *   on sessions, in provider names, in `authRequired` lists, in route
 *   prefixes. One handler type can back several schemes.
 * - A **credential** is whatever a request carries to prove identity: a cookie,
 *   a bearer token. A handler that hands one out implements `signIn`; Wasp
 *   ships two such handlers, `waspBearer()` and `waspCookie()`, and runs one
 *   privately for every scheme with inline `credentials`. A handler that
 *   only verifies logins asks Wasp for a `CredentialsIssuer` (see
 *   `WaspServerRuntime.credentialsIssuer`).
 */

export type * from "./typedAdapter.js";

export type JsonValue =
  | string
  | number
  | boolean
  | null
  | JsonValue[]
  | { [key: string]: JsonValue };

/**
 * THE REQUEST BINDING
 * Nothing in this contract takes the incoming request as an option. Wasp
 * binds the request to the whole call chain it starts (Node's
 * `AsyncLocalStorage`), at the one place it owns: the Express boundary. The
 * app's hooks receive it as `req`, `merge` checks it, and a handler never
 * threads it through. Outside a request (a job, a script) the hooks see
 * `req: undefined`.
 */

/**
 * A reference to one `AuthIdentity` row, by its primary key as a handler
 * sees it. The key is (`handlerName`, `providerName`, `providerUserId`); a
 * handler gives the last two, and Wasp adds the first: always the calling
 * handler's own name, never one the handler supplies. That is what makes
 * acting on another handler's identities unrepresentable.
 *
 * A REFERENCE and not the key itself, because the row need not exist yet: a
 * handler names an identity before Wasp has provisioned it. A `providerName`
 * the manifest did not declare is rejected before any lookup.
 */
export type AuthIdentityRef<ProviderName extends string = string> = {
  /** One of the manifest's `providers` (`email`): the `providerName` column. Default: `default`. */
  providerName?: ProviderName;
  /** The handler's STABLE id for the person under that provider name: an email, a provider's user id. */
  providerUserId: string;
};

/**
 * The full primary key of an `AuthIdentity` row: what an issuer receives, so
 * it can find the identity without being told separately who verified the
 * login. Wasp fills `handlerName` with the calling scheme; a handler never
 * chooses it.
 */
export type AuthIdentityKey = {
  handlerName: string;
  providerName: string;
  providerUserId: string;
};

/**
 * What a CREDENTIAL handler learned from a credential it owns: which of ITS
 * identities the request is from, and what it knows about that credential.
 */
export type IdentityPrincipal = AuthIdentityRef & {
  /**
   * Verified facts about the person (email, name), as plain JSON. Wasp stores
   * them on the identity when it first creates the user, and hands them to
   * the app's `userSignupFields`.
   */
  claims?: Record<string, JsonValue>;
  /** The handler's id for THIS credential (a session id), so logout can revoke exactly this one. */
  credentialId?: string;
  /** When the credential was issued, if the handler knows. */
  credentialIssuedAt?: Date;
  /** Whether the credential is younger than the scheme's `freshFor`, if the handler knows. */
  isCredentialFresh?: boolean;
};

/**
 * What Wasp knows about the request's credential once it resolved it to an
 * ACCOUNT: from its own issuer directly, or from a credential handler's
 * `IdentityPrincipal` after provisioning. What `runtime.authenticate`
 * returns, and what the imperative API and `merge` reason about.
 */
export type AccountPrincipal = {
  authId: string;
  credentialId?: string;
  /** Null for a handler-owned credential the handler said nothing about. */
  credentialIssuedAt: Date | null;
  /** False when unknown. */
  isCredentialFresh: boolean;
};

/**
 * The answer to "whose request is this?" from a credential handler. An unknown
 * caller is a normal answer, not an error. A handler whose credential carries
 * Wasp's account key (Wasp's own issuers) answers with the account, and may
 * say which scheme verified the login; every other handler answers with one
 * of its identities.
 */
export type AuthenticateResult =
  | { status: "authenticated"; principal: IdentityPrincipal }
  | { status: "authenticated"; account: AccountPrincipal; signedInBy?: string }
  | { status: "unauthenticated" };

/** The manifest's `providers`: one declaration per provider name. */
export type ProviderDeclarations = {
  [providerName: string]: ProviderDeclaration;
};

/** What a handler declares about one of its providers. */
export type ProviderDeclaration = {
  /**
   * What a login through this provider CARRIES besides the identity itself.
   * Omitted: nothing (a password, a magic link, a passkey, another system's
   * session). The kind is not a taxonomy of login methods: only a kind that
   * brings data of its own exists, because that data is all it changes.
   *
   * - "oauth": the provider's tokens. Every signup, login and link through
   *   this provider must hand them to Wasp (`oauth`, see `OAuthLoginData`),
   *   and the app's hooks receive them. Leaving them out is a type error for
   *   a typed adapter, and `wasp-auth/missing-oauth-data` at runtime.
   */
  kind?: ProviderKind;
};

/** See `ProviderDeclaration.kind`. */
export type ProviderKind = "oauth";

/**
 * The tokens an OAuth provider handed over. The four named fields are what
 * every provider can give; anything else the provider returned (a token
 * type, granted scopes, a provider-specific id) rides along under its own
 * name, untyped.
 */
export type OAuthTokens = {
  accessToken: string;
  refreshToken?: string | null;
  idToken?: string | null;
  accessTokenExpiresAt?: Date | null;
  [providerSpecificField: string]: unknown;
};

/**
 * What a login through an `"oauth"` provider carries. The handler hands it to
 * Wasp with every `create`, `provision`, `link` and `signIn` of such a
 * provider, and Wasp hands it to the app's hooks as `oauth` (`OAuthData`),
 * adding the provider's name.
 */
export type OAuthLoginData = {
  /** The id the app saw in `onBeforeOAuthRedirect`, so it can match the redirect to this login. */
  uniqueRequestId: string;
  tokens: OAuthTokens;
};

/**
 * A provider's kind as a type parameter: `"oauth"`, `undefined` for a
 * provider that declares none, or BOTH for "not known here", which is the
 * loose default a hand-written handler gets.
 */
export type ProviderKindParam = ProviderKind | undefined;

/**
 * The data a call must carry, by the provider's kind: REQUIRED for "oauth",
 * not accepted for a provider without a kind, optional when the kind is not
 * known to the type system.
 */
export type LoginData<Kind extends ProviderKindParam = ProviderKindParam> = [
  Kind,
] extends ["oauth"]
  ? { oauth: OAuthLoginData }
  : [Kind] extends [undefined]
    ? { oauth?: never }
    : { oauth?: OAuthLoginData };

/**
 * Per-sign-in choices, decided by the handler that verified the login
 * (ASP.NET's `AuthenticationProperties`). Everything is optional; an absent
 * property means the credentials scheme's own configuration applies.
 */
export type SignInProperties = {
  /**
   * Lifetime of THIS credential, e.g. `"12h"` or `"90d"`, instead of the
   * credentials scheme's configured `ttl`.
   */
  ttl?: string;
  /**
   * Whether the credential should outlive the browser session ("remember
   * me"). Default: true. When false, a cookie credential is a session cookie,
   * and a bearer credential is kept by the generated client in
   * `sessionStorage`. The server-side lifetime is still bounded by `ttl`.
   */
  persistent?: boolean;
};

/** What a credential issuer produced for the browser to carry. */
export type SignInResult = {
  /** What to send the client: a body carrying a token, a Set-Cookie header, ... A standard `Response`. */
  response: Response;
  /** The issuer's own id for the credential, for later revocation. */
  credentialId?: string;
};

/**
 * What Wasp's internals need from a CREDENTIAL handler: how Wasp recognises
 * a credential the handler owns. Wasp's own issuers are credential handlers
 * too, with no login of their own. A login handler implements none of this;
 * the issuer it signs into does.
 *
 * `authenticate` is the only required operation: the request read path, the
 * one every scheme answers. Everything else is optional, and presence IS the
 * capability -- Wasp detects what a handler can do by which methods exist and
 * falls back to its own defaults otherwise (a 401 for `challenge`, a 403 for
 * `forbid`, "clear the client side" for `signOut`).
 */
export interface CredentialHandler {
  /**
   * Authenticate an incoming request.
   *
   * Returns `{ status: "unauthenticated" }` when the request carries no valid
   * credential. That is *not* an error -- Wasp lets unauthenticated requests
   * through and leaves it to individual assets to decide whether they require
   * a user.
   *
   * The request is a standard web `Request`. For plain HTTP traffic Wasp builds it
   * from the incoming request, headers and all. For websocket authentication Wasp
   * synthesizes one carrying only an `Authorization: Bearer <credential>` header --
   * a handler that wants websocket support must be able to authenticate from
   * headers alone.
   */
  authenticate(request: Request): Promise<AuthenticateResult>;

  /**
   * Issue a credential for an identity: the handler is a credential issuer
   * other schemes can sign into (`credentials: { scheme: "<this one>" }`,
   * `runtime.credentialsIssuerFor`). The identity is the calling scheme's,
   * already provisioned, with `handlerName` filled in by Wasp; Wasp guards
   * the provider name and fires the app's login hooks before calling in.
   */
  signIn?(
    identity: AuthIdentityKey,
    properties?: SignInProperties,
  ): Promise<SignInResult>;

  /**
   * Invalidate the credential the request carries. What to send the client
   * (an expired cookie, nothing at all) is the handler's business.
   */
  signOut?(request: Request): Promise<Response>;

  /**
   * End EVERY credential of the person behind one of this handler's
   * identities: what the imperative `signOutEverywhere(user)` calls for a
   * handler that keeps its own credentials (Better Auth's sessions, Clerk's).
   * Wasp's own issuers implement it; a handler whose credentials Wasp
   * issues omits it.
   */
  signOutEverywhere?(identity: AuthIdentityKey): Promise<void>;

  /**
   * What to send a request that needs a user and has none. A cookie handler
   * redirects to a login page; a bearer handler answers 401. Default: 401.
   */
  challenge?(request: Request): Promise<Response>;

  /**
   * What to send an authenticated request that is not allowed in. Default: 403.
   */
  forbid?(request: Request): Promise<Response>;
}

/**
 * Runtime facets a handler may request from Wasp through its manifest's
 * `uses` list.
 *
 * A closed set on purpose: the generator wires only the facets it knows, so an
 * unknown name is a compile error rather than an absent property at runtime.
 * Requesting a grant is also an audit surface -- a reviewer reads `uses: [...]`
 * in the manifest and knows the handler's blast radius.
 *
 * Credentials are not a grant: a manifest's `credentials` field is what asks
 * for the `credentialsIssuer`.
 */
export type RuntimeGrantName = "email-send";

/** The options of `CredentialsIssuer.signIn`. */
export type SignInOpts = {
  properties?: SignInProperties;
  /** Skip the login hooks: for flows that already fired them. */
  skipHooks?: boolean;
};

/**
 * The credentials issuer: how a handler that verifies logins but cannot carry a
 * credential across requests hands one out.
 *
 * Wasp builds it from the manifest's `credentials` field -- an inline issuer
 * (`{ transport, store }`) or a sibling scheme (`{ scheme }`) -- and the
 * handler cannot tell the two apart. `signIn` resolves the subject through the
 * calling scheme's OWN declared provider names, fires the app's `onBeforeLogin`
 * (a throw vetoes) and `onAfterLogin` hooks, and stamps the calling scheme as
 * `signedInBy` on whatever the issuer produces. Minting through this facet is
 * the choke point that guarantees no scheme skips the app's login policy.
 */
export type CredentialsIssuer<
  Kinds extends { [providerName: string]: ProviderKindParam } = {
    [providerName: string]: ProviderKindParam;
  },
> = {
  /**
   * Issues a credential for a login the handler just verified. The provider's
   * kind is looked up from `identityRef.providerName`: for an "oauth"
   * provider the options are required and carry the `oauth` data.
   */
  signIn<ProviderName extends keyof Kinds & string>(
    identityRef: AuthIdentityRef<ProviderName>,
    ...rest: OptsArg<
      SignInOpts & LoginData<Kinds[ProviderName]>,
      Kinds[ProviderName]
    >
  ): Promise<SignInResult>;

  /** Invalidate the credential the request carries, through the issuer. */
  signOut(request: Request): Promise<Response>;

  /**
   * Invalidate every credential of the person behind this subject that the
   * issuer can find (the password-rotation semantic). Wasp-side only: it does
   * NOT call back into the calling handler, so a handler may call it from
   * inside its own revocation path without recursion.
   */
  signOutEverywhere(
    identityRef: AuthIdentityRef<keyof Kinds & string>,
  ): Promise<void>;
};

/**
 * The `email-send` grant: send through the app's configured `emailSender`.
 *
 * Requesting it is a compile-time claim -- Wasp rejects the manifest when the
 * app has no `emailSender` -- so an OTP or magic-link handler can never ship
 * into an app that silently drops its emails. SMTP credentials never reach
 * the handler; only the send capability does.
 */
export type WaspEmail = {
  send(email: {
    to: string;
    from?: EmailFrom;
    subject: string;
    text: string;
    html: string;
  }): Promise<void>;
  /** The app-level default sender (`app.emailSender.defaultFrom`), if configured. */
  defaultFrom?: EmailFrom;
};

export type EmailFrom = { name?: string; email: string };

/**
 * Error codes the granted facets reject with.
 *
 * Codes rather than error classes on purpose: handler packages hold their own
 * copy of this contract, and `instanceof` does not survive package-copy
 * boundaries (tsc unifies by name@version, Node does not).
 */
export type AuthContractErrorCode =
  | "wasp-auth/duplicate-identity"
  | "wasp-auth/identity-not-found"
  /** A signup, login or link through an `"oauth"` provider came without its `oauth` data. */
  | "wasp-auth/missing-oauth-data"
  /** `createOneTimeCode` was given a request that carries no valid credential. */
  | "wasp-auth/unauthenticated"
  | "wasp-auth/undeclared-provider-name"
  /**
   * A facet was used that the manifest did not declare: `credentialsIssuer` without
   * a `credentials` config, `email` without the `"email-send"` grant.
   */
  | "wasp-auth/undeclared-facet"
  /**
   * `link` found the subject's identity already attached to a DIFFERENT
   * account. Deliberately carries nothing about that account.
   */
  | "wasp-auth/identity-linked-elsewhere"
  /** `unlink` refused to remove an account's only identity. */
  | "wasp-auth/last-identity"
  /** `merge` was called in an app that declares no `auth.mergeUsers`. */
  | "wasp-auth/merging-disabled"
  /** `merge` was requested without a fresh credential of the surviving account. */
  | "wasp-auth/credential-not-fresh"
  /**
   * The app's onBeforeSignup/onBeforeLogin hook rejected the action by
   * throwing. The thrown error itself is what carries this code (Wasp tags
   * it rather than wrapping, so its message and type survive) -- a handler's
   * routes should map it to a 4xx carrying `error.message`, not to a 500.
   */
  | "wasp-auth/policy-veto";

/** The code of a granted-facet error, or null for any other value. */
export function getAuthContractErrorCode(
  error: unknown,
): AuthContractErrorCode | null {
  if (typeof error !== "object" || error === null || !("code" in error)) {
    return null;
  }
  const code = (error as { code: unknown }).code;
  return code === "wasp-auth/duplicate-identity" ||
    code === "wasp-auth/identity-not-found" ||
    code === "wasp-auth/missing-oauth-data" ||
    code === "wasp-auth/unauthenticated" ||
    code === "wasp-auth/undeclared-provider-name" ||
    code === "wasp-auth/undeclared-facet" ||
    code === "wasp-auth/identity-linked-elsewhere" ||
    code === "wasp-auth/last-identity" ||
    code === "wasp-auth/merging-disabled" ||
    code === "wasp-auth/credential-not-fresh" ||
    code === "wasp-auth/policy-veto"
    ? code
    : null;
}

/**
 * Everything Wasp hands a server-side handler about the app it runs in.
 *
 * This is the handler's *only* window into the app: handlers must not import
 * generated code (`wasp/...`) and must not read `process.env` themselves. Keeping
 * the boundary here is what lets an handler package typecheck and version
 * independently of any particular Wasp app.
 *
 * Every facet is ALWAYS a member, whatever the manifest declared. One the
 * manifest did not ask for still exists, and rejects with
 * `wasp-auth/undeclared-facet`, naming the scheme and the missing declaration,
 * the moment it is used. That is deliberate:
 * - A handler's type never has to claim what its manifest declares. Such a
 *   claim could not be checked (the manifest is a value in the app's
 *   `main.wasp.ts`, the handler is typed in another package), and a wrong one
 *   used to surface as "cannot read properties of undefined" at first login.
 * - Whether a facet is available is often the APP's decision (the email
 *   method is on or off; a handler lets the app opt into Wasp-issued
 *   credentials). A handler branches on `hasCredentialsIssuer` / `canSendEmail`.
 *
 * `ProviderNames` is the union of the manifest's `providers` names;
 * it types the keys of `identities`.
 */
export type WaspServerRuntime<ProviderNames extends string = "default"> = {
  /** The name of this scheme, as declared in the app's `auth.schemes`. */
  scheme: string;

  /**
   * Where this scheme's routes are mounted on the Wasp server:
   * `/auth/<scheme>`. The handler's route paths are relative to it, and its
   * redirect URIs (OAuth callbacks) are built on it.
   */
  mountPath: string;

  /**
   * The app's PrismaClient instance. Typed as `unknown` because the client's type
   * is generated per app; handlers that need it narrow it themselves.
   */
  db: unknown;

  /**
   * The Prisma datasource provider of the app's database: `"sqlite"`,
   * `"postgresql"`, ... Handlers that bring their own storage layer (Better
   * Auth's prisma adapter, for one) need to know the dialect they are talking to.
   */
  dbProvider: string;

  /**
   * The server-side environment, already validated against the env vars the
   * handler's manifest declared.
   */
  env: Record<string, string | undefined>;

  /** The URL the Wasp server is reachable at. */
  serverUrl: string;

  /** The URL the Wasp client is served from. Useful for trusted-origin checks. */
  clientUrl: string;

  /**
   * Whether the app runs in development mode. For dev-only conveniences and
   * the `secure` flag on any cookies the handler's routes set.
   */
  isDevelopment: boolean;

  /**
   * Whether the app declares `auth.mergeUsers`. A handler offers the merge
   * step of its linking flow only when this is true; otherwise a login that
   * belongs to another account simply cannot be linked.
   */
  isAccountMergingEnabled: boolean;

  /**
   * Who sent this request, as Wasp sees it: Wasp's own credential first, then
   * the handler's `authenticate` (a credential handler), resolved to the
   * ACCOUNT, provisioning a never-seen identity exactly like the middleware
   * does. For a handler's own routes that act as the signed-in user
   * (linking). Null for nobody.
   */
  authenticate(request: Request): Promise<AccountPrincipal | null>;

  /**
   * A one-time code: a short-lived (one minute), single-use stand-in for the
   * account behind the credential `request` carries, safe to put in a URL.
   * For a browser NAVIGATION to one of the handler's own routes made as the
   * signed-in user ("connect Google to my account", a download): a
   * navigation cannot carry an `Authorization` header, so a bearer
   * credential would not arrive. Works for every scheme, whoever owns the
   * credential: Wasp keeps the codes in a table of its own.
   *
   * Resolves to null when no code is needed, because the request was
   * authenticated by a Wasp cookie and the navigation carries it by itself.
   * So the handler never has to know the transport: it puts the code in the
   * URL when it got one. Rejects with `wasp-auth/unauthenticated` when
   * `request` carries no valid credential.
   *
   * A one-time code is not a credential: it says who, not how recently they
   * logged in, and `authenticate` never accepts it.
   */
  createOneTimeCode(request: Request): Promise<string | null>;

  /**
   * The account a one-time code stands for. Spends the code: an unknown,
   * expired or already spent one is null.
   */
  redeemOneTimeCode(oneTimeCode: string): Promise<AccountPrincipal | null>;

  /**
   * One store per declared provider name:
   * `identities.email.find(...)`, `identities.google.create(...)`. A manifest
   * that declares no `providers` gets exactly one, self-assigned:
   * `identities.default`. Every store is bound to this scheme: Wasp writes
   * its name to the identity's `handlerName` column, next to the
   * `providerName`. The keys are the boundary: a provider name the manifest did
   * not declare has no member here. Each store is the sanctioned channel for
   * everything identity-shaped, with the same powers Wasp's own auth uses.
   *
   * - `provision` is the eager-provisioning channel: an in-process handler
   *   that observes its own signup moment (Better Auth can; a hosted provider
   *   cannot) reports it here, so the local user exists from signup rather
   *   than from the first authenticated request. Idempotent.
   * - `data`/`secrets` accessors let a handler keep per-identity state
   *   without touching `db`: non-secret working state in `data`, secret
   *   material in `secrets` -- a column the app's Prisma client omits by
   *   default, so it cannot leak through app code. Secrets are stored as
   *   given; hashing is the handler's job.
   */
  identities: { readonly [ProviderName in ProviderNames]: IdentityStore };

  /**
   * How the scheme signs people in: the issuer behind the manifest's
   * `credentials` (a private one, or a sibling scheme). Always a member; when
   * the manifest declares no `credentials`, every method rejects with
   * `wasp-auth/undeclared-facet`. Check {@link hasCredentialsIssuer} first when that
   * is the app's choice rather than the handler's.
   */
  credentialsIssuer: CredentialsIssuer;

  /** Whether the manifest declares `credentials`, so {@link credentialsIssuer} works. */
  hasCredentialsIssuer: boolean;

  /**
   * The issuer of ANY scheme whose handler implements `signIn`, for a handler
   * that signs into a scheme other than its configured default
   * (`credentialsIssuer`). Same facet, same guards: only this scheme's own
   * identities, the app's login hooks included. Throws when no such scheme
   * exists or it cannot issue.
   */
  credentialsIssuerFor(scheme: string): CredentialsIssuer;

  /**
   * The app's configured email sender. Always a member; `send` rejects with
   * `wasp-auth/undeclared-facet` unless the manifest requests the
   * `"email-send"` grant in `uses`. SMTP credentials never reach the handler.
   */
  email: WaspEmail;

  /** Whether the manifest requests `"email-send"`, so {@link email} works. */
  canSendEmail: boolean;
};

/** The three channels of an identity row. */
export type IdentityContent = {
  /** What the handler VERIFIED about the person (email, name). Written once. */
  claims?: Record<string, JsonValue>;
  /** The handler's non-secret working state (`isEmailVerified`, timestamps). */
  data?: Record<string, JsonValue>;
  /** Secret material (a password hash), in a column the Prisma client omits by default. */
  secrets?: Record<string, JsonValue>;
};

/** Computes the app's user fields for a signup. A callback, so the app's veto runs before it does. */
export type GetUserFields = () =>
  | Promise<Record<string, JsonValue>>
  | Record<string, JsonValue>;

/**
 * One options object after the id, for every identity write. The kind of the
 * provider decides whether it may be OMITTED: for an "oauth" provider the
 * options carry the tokens, so they are required; otherwise `create(id)` is
 * a complete call.
 */
export type OptsArg<Opts, Kind extends ProviderKindParam> = [Kind] extends [
  "oauth",
]
  ? [opts: Opts]
  : [opts?: Opts];

/** The options of `IdentityStore.create`. */
export type CreateOpts<Kind extends ProviderKindParam> = {
  identity?: IdentityContent;
  /** Omitted: the manifest's `userFieldsFromClaims` run over the claims instead. */
  getUserFields?: GetUserFields;
  /** Skip the signup hooks: for identity writes that are not a signup (an import). */
  skipHooks?: boolean;
} & LoginData<Kind>;

/** The options of `IdentityStore.provision`. */
export type ProvisionOpts<Kind extends ProviderKindParam> = {
  identity?: IdentityContent;
} & LoginData<Kind>;

/** The options of `IdentityStore.reportLogin`: only what the provider's kind requires. */
export type ReportLoginOpts<Kind extends ProviderKindParam> = LoginData<Kind>;

/** The options of `IdentityStore.link`. `authId` names the account the identity attaches to. */
export type LinkOpts<Kind extends ProviderKindParam> = {
  authId: string;
  identity?: IdentityContent;
} & LoginData<Kind>;

/**
 * The per-scheme view of Wasp's identity store. `providerUserId` is always the
 * handler's own stable user id -- the same value {@link Principal} carries.
 */
export type IdentityStore<Kind extends ProviderKindParam = ProviderKindParam> =
  {
    /** The identity (claims and non-secret data), or null if never provisioned. */
    find(providerUserId: string): Promise<{
      authId: string;
      claims: Record<string, JsonValue>;
      data: Record<string, JsonValue>;
    } | null>;

    /**
     * Idempotent create of the local user for a subject. Runs the app's
     * `userSignupFields` over the claims, exactly like just-in-time
     * provisioning at first authentication.
     */
    provision(
      providerUserId: string,
      ...rest: OptsArg<ProvisionOpts<Kind>, Kind>
    ): Promise<{ authId: string }>;

    /**
     * Strict create of the local user for a subject: signup semantics, where
     * `provision` is login semantics. Rejects with
     * `wasp-auth/duplicate-identity` when the subject already exists.
     *
     * `opts.getUserFields` computes the new user entity's own fields; it is a
     * callback (not a value) so the provisioning layer controls when it runs --
     * the app's signup veto, once it fires at this choke point, must run before
     * any user-supplied field getters do. When omitted, the scheme's
     * manifest's `userFieldsFromClaims` run over the claims instead.
     */
    create(
      providerUserId: string,
      ...rest: OptsArg<CreateOpts<Kind>, Kind>
    ): Promise<{ authId: string }>;

    /**
     * Delete ONE identity, the handler's own decision (an unverified signup
     * that was superseded, say). Resolves to whether there was one.
     *
     * The app's user goes with it ONLY when this was the account's last
     * identity: an account nobody can log into is not kept. An account that
     * has other identities (a linked Google login) keeps them, its user and
     * everything the user owns. One transaction.
     *
     * Compare `unlink`, the person's own request, which REFUSES the last
     * identity instead.
     */
    delete(providerUserId: string): Promise<boolean>;

    /**
     * Account linking: attach a new identity to an EXISTING account, instead of
     * creating a user. `authId` is the account to attach to -- for a request
     * carrying a Wasp-issued credential, the `principal.providerUserId` that
     * `runtime.credentialsIssuer.authenticate` returns.
     *
     * Wasp checks that the account already carries an identity in one of the
     * calling scheme's OWN provider names (a scheme cannot attach itself to another
     * scheme's users), fires the app's `onBeforeLink` (a throw vetoes) and
     * `onAfterLink` hooks, and never runs `userSignupFields`: the user
     * already exists. Idempotent when the identity is already on that account.
     * Rejects with `wasp-auth/identity-linked-elsewhere` when it belongs to a
     * different one.
     */
    link(providerUserId: string, opts: LinkOpts<Kind>): Promise<void>;

    /**
     * Detach the subject's identity from the account. Rejects with
     * `wasp-auth/identity-not-found` when the account does not hold it, and
     * with `wasp-auth/last-identity` when it is the account's only way in.
     */
    unlink(providerUserId: string, opts: { authId: string }): Promise<void>;

    /**
     * Account merging: make two accounts one. In a single transaction Wasp
     * calls the app's `auth.mergeUsers` function (only the app knows how to
     * combine its own data), moves every identity of `fromAuthId` onto
     * `intoAuthId`, and deletes the `from` user -- which ends its credentials
     * too. Anything throwing rolls the whole merge back.
     *
     * The handler must have PROVEN control of both accounts before calling
     * this: the `into` account by its credential, the `from` account by a
     * fresh login (a verified password, a completed OAuth flow). Wasp checks
     * that both accounts carry an identity in the calling scheme's OWN
     * provider names. Rejects with `wasp-auth/merging-disabled` when the app
     * declares no `auth.mergeUsers`.
     */
    merge(opts: { fromAuthId: string; intoAuthId: string }): Promise<void>;

    /**
     * A login through a credential the handler OWNS (Better Auth's session,
     * Clerk's), reported so the app's login hooks fire: `onBeforeLogin` (a
     * throw refuses the login; the handler must not create its session then)
     * and `onAfterLogin`. A handler with Wasp credentials never calls this:
     * `credentialsIssuer.signIn` fires them. For an "oauth" provider the
     * tokens are required, as for `create`.
     */
    reportLogin(
      providerUserId: string,
      ...rest: OptsArg<ReportLoginOpts<Kind>, Kind>
    ): Promise<void>;

    /**
     * Merges the updates into the identity's non-secret data. A key set to
     * `null` is REMOVED; every key not named is left alone. Atomic: two
     * concurrent updates cannot lose one another.
     */
    updateData(
      providerUserId: string,
      updates: Record<string, JsonValue>,
    ): Promise<void>;

    /** Reads the identity's secret material. Keep the result on the server. */
    getSecrets(
      providerUserId: string,
    ): Promise<Record<string, JsonValue> | null>;

    /**
     * Merges the updates into the identity's secret material, exactly like
     * `updateData`: `null` removes a key, unnamed keys are left alone. So
     * changing a password cannot wipe a second secret (a TOTP seed) stored next
     * to it. Expects the values already hashed.
     */
    updateSecrets(
      providerUserId: string,
      updates: Record<string, JsonValue>,
    ): Promise<void>;
  };

/**
 * What a server adapter returns: the `CredentialHandler` Wasp recognises the
 * handler's own credential through, and its routes. `credentialHandler` may be omitted
 * only when the manifest declares `credentials` and the handler has no
 * credential of its own: Wasp then recognises, ends and challenges for the
 * credential it issued, and the handler is its routes.
 *
 * Deliberately a WRAPPER around `CredentialHandler` rather than `CredentialHandler` with
 * a `routeHandler` member added: `CredentialHandler` stays exactly the interface
 * Wasp depends on, an adapter can wrap a handler object some library built
 * without mutating it, and once Wasp has full-stack modules the routes can
 * move out without touching `CredentialHandler`.
 */
export type ServerAuthHandlerParts = {
  credentialHandler?: CredentialHandler;
  /** The handler's own routes (login, signup, callbacks), mounted at `/auth/<name>`. */
  routeHandler?: (request: Request) => Response | Promise<Response>;
};

/**
 * The server adapter: what a package exports as `createServerAuthHandler`,
 * or a hand-written handler references from `main.wasp.ts`. Wasp calls it
 * once at server start with the runtime and the manifest's `server.spec`
 * (references replaced by the live functions). This is the loose form; a
 * package with a spec constructor uses `ServerAuthAdapterFor<typeof myAuth>`,
 * which reads off the manifest what the adapter must return.
 */
export type ServerAuthAdapter<
  ServerSpec = unknown,
  ProviderNames extends string = "default",
> = (
  runtime: WaspServerRuntime<ProviderNames>,
  spec: ServerSpec,
) => ServerAuthHandlerParts | Promise<ServerAuthHandlerParts>;

// ---------------------------------------------------------------------------
// Credential issuers: what backs an inline `credentials: { transport, store }`.
// Wasp ships the transports (bearer, cookie) and the stores (prisma, signed
// token); the interfaces are public so an app can bring its own store.
// ---------------------------------------------------------------------------

/** What a credential issuer keeps per issued credential. */
export type CredentialRecord = {
  /** The `Auth` entity id of the person the credential belongs to. */
  authId: string;
  /** The scheme that verified the login. */
  signedInBy: string;
  issuedAt: Date;
  expiresAt: Date;
};

/**
 * Where a credential issuer keeps its records. The Wasp-shipped stores:
 *
 * - `prisma`: a row per credential in the injected `Session` model. Immediate
 *   revocation, `signOutEverywhere` by deleting rows.
 * - `signed-token`: the record travels inside a signed token. No table; a
 *   token stays valid until it expires (`delete` is a no-op) and
 *   `signOutEverywhere` works by stamping the subject, so tokens issued before
 *   the stamp stop validating.
 *
 * An app can supply its own (Redis, its own Prisma model) through the
 * manifest's `credentials.store` reference.
 */
export type CredentialStore = {
  create(record: CredentialRecord): Promise<{ id: string }>;
  /** The live record, or null when unknown, expired or revoked. */
  get(id: string): Promise<CredentialRecord | null>;
  delete(id: string): Promise<void>;
  deleteAllForAuthId(authId: string): Promise<void>;
  /**
   * Move a credential's expiry, for sliding renewal (`credentials.slidingRenewal`).
   * Optional: a store whose credential cannot be changed after it was issued
   * (a signed token) omits it, and cannot be configured with sliding renewal.
   */
  extend?(id: string, expiresAt: Date): Promise<void>;
};
