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
 * - A **handler** is the code: an `AuthHandler` object. An handler package
 *   implements one in its server entry and exposes it as a named
 *   `createServerAuthHandler` export (see `ServerAuthAdapter`).
 * - A **scheme** is a named, configured instance of a handler, declared in the
 *   app's `auth.schemes` map. The scheme name is what Wasp records everywhere:
 *   on sessions, in provider names, in `authRequired` lists, in route
 *   prefixes. One handler type can back several schemes.
 * - A **credential** is whatever a request carries to prove identity: a cookie,
 *   a bearer token. A handler that needs to hand one out asks Wasp for a
 *   `CredentialsIssuer` (see `WaspServerRuntime.credentialsIssuer`).
 */

import type { IncomingMessage, ServerResponse } from "node:http";

export type * from "./typedAdapter.js";

export type JsonValue =
  | string
  | number
  | boolean
  | null
  | JsonValue[]
  | { [key: string]: JsonValue };

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
 * The result of successfully authenticating a request.
 *
 * NOTE: the primitive here is *verify*, not *fetch*. A handler turns a
 * credential that arrived with the request into a subject. It is deliberately
 * NOT `findById(id)`: a hosted provider (Clerk) validates a signed token and
 * has no way to look a subject up by id on our behalf.
 */
export type Principal = AuthIdentityRef & {
  /**
   * Verified profile data about the subject, as far as the handler knows it:
   * email, name, avatar, whatever the verified token or session carried.
   *
   * Wasp feeds this to the app's `userSignupFields` when it provisions a local
   * user for a subject it has not seen before, and records it on the identity
   * it creates. Omit rather than invent: an absent claim is recoverable, a
   * made-up one is not.
   */
  claims?: Record<string, JsonValue>;

  /**
   * The scheme that verified the login this credential descends from, when
   * the authenticating scheme is a credential issuer signed into by another
   * scheme (Wasp's own auth signing into a cookie scheme, say). Defaults to
   * the authenticating scheme itself. Recorded as `user.signedInBy`.
   */
  signedInBy?: string;

  /**
   * Opaque, handler-owned id of the credential that authenticated the request
   * (a session row id, typically). Wasp hands it back to `signOut`-shaped
   * flows and uses it for diagnostics; it never interprets its contents.
   */
  credentialId?: string;
};

/**
 * The outcome of authenticating a request.
 *
 * Deliberately a tagged union rather than `Principal | null`: call sites
 * read as prose, and future outcomes (an explicit "invalid credential" state,
 * say) become additive union members instead of signature breaks.
 */
export type AuthenticateResult =
  | { status: "authenticated"; principal: Principal }
  | { status: "unauthenticated" };

/**
 * A minimal Response the framework can send: status, headers, and an optional
 * JSON body. Kept framework-agnostic on purpose (no Express types), and small
 * enough for a handler to build by hand.
 */
export type AuthResponse = {
  status: number;
  headers?: Record<string, string | string[]>;
  body?: JsonValue;
};

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

/** The tokens an OAuth provider handed over. A provider's extra fields ride along untyped. */
export type OAuthTokens = {
  accessToken: string;
  refreshToken?: string | null;
  idToken?: string | null;
  accessTokenExpiresAt?: Date | null;
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

/** What a handler receives when asked to sign a subject in. */
export type SignInContext = {
  /** Per-sign-in choices of the handler that verified the login. */
  properties?: SignInProperties;
  /** The scheme that verified the login, pre-bound by Wasp. Never forgeable. */
  signedInBy: string;
  /** The incoming request, when the sign-in happens inside one. */
  req?: unknown;
};

/** What a credential issuer produced for the browser to carry. */
export type SignInResult = {
  /** What to send the client: a body carrying a token, a Set-Cookie header, ... */
  response: AuthResponse;
  /** The issuer's own id for the credential, for later revocation. */
  credentialId?: string;
};

/**
 * What Wasp's internals need from an auth handler.
 *
 * `authenticate` is the only required operation: the request read path, the
 * one every scheme answers. Everything else is optional, and presence IS the
 * capability -- Wasp detects what a handler can do by which methods exist and
 * falls back to its own defaults otherwise (a 401 for `challenge`, a 403 for
 * `forbid`, "clear the client side" for `signOut`).
 */
export interface AuthHandler {
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
   * Issue a credential for a subject: the handler is a credential issuer other
   * schemes can sign into (`credentials: { scheme: "<this one>" }`). The
   * subject is one of THIS scheme's, already provisioned; Wasp guards the
   * provider name and fires the app's login hooks before calling in.
   */
  signIn?(
    identityRef: AuthIdentityRef,
    context: SignInContext,
  ): Promise<SignInResult>;

  /**
   * Invalidate the credential the request carries. What to send the client
   * (an expired cookie, nothing at all) is the handler's business.
   */
  signOut?(request: Request): Promise<AuthResponse>;

  /**
   * What to send a request that needs a user and has none. A cookie handler
   * redirects to a login page; a bearer handler answers 401. Default: 401.
   */
  challenge?(request: Request): Promise<AuthResponse>;

  /**
   * What to send an authenticated request that is not allowed in. Default: 403.
   */
  forbid?(request: Request): Promise<AuthResponse>;
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
  /** The incoming request, passed to the app's login hooks. */
  req?: unknown;
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
   * Authenticate a request against the credential this scheme hands out:
   * the target issuer's own `authenticate`. A handler that verifies logins
   * but carries no credential of its own forwards its `authenticate` here
   * (ASP.NET's remote schemes forward to their sign-in scheme the same way),
   * so `authRequired: ["<this scheme>"]` recognizes the credential it issued.
   */
  authenticate(request: Request): Promise<AuthenticateResult>;

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
  signOut(request: Request): Promise<AuthResponse>;

  /**
   * Invalidate every credential of the person behind this subject that the
   * issuer can find (the password-rotation semantic). Wasp-side only: it does
   * NOT call back into the calling handler, so a handler may call it from
   * inside its own revocation path without recursion.
   */
  signOutEverywhere(
    identityRef: AuthIdentityRef<keyof Kinds & string>,
  ): Promise<void>;

  /**
   * A one-time code: a short-lived (one minute), single-use stand-in for the
   * credential `request` carries, safe to put in a URL. For a browser
   * NAVIGATION to one of the handler's own routes made as the signed-in user
   * ("connect Google to my account"): a navigation cannot carry an
   * `Authorization` header, so a bearer credential would not arrive.
   *
   * Resolves to null when no code is needed, because the credential is a
   * cookie and the navigation carries it by itself. So the handler never has
   * to know the transport: it puts the code in the URL when it got one.
   * Rejects with `wasp-auth/unauthenticated` when `request` carries no valid
   * credential.
   *
   * Same idea, same name and same replay protection (the `UsedOneTimeCode`
   * model) as the one-time code that ends an OAuth login, in the other
   * direction: that one carries a credential OUT of a navigation, this one
   * carries it IN. A one-time code is not a credential: `authenticate` never
   * accepts it.
   */
  createOneTimeCode(request: Request): Promise<string | null>;

  /**
   * Who a one-time code stands for, as `authenticate` would have answered for
   * the request the code was created from. Spends the code: an unknown,
   * expired or already spent one is `unauthenticated`.
   */
  redeemOneTimeCode(oneTimeCode: string): Promise<AuthenticateResult>;
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
  /** `credentials.createOneTimeCode` was given a request that carries no valid credential. */
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
  /** The incoming request, passed to the app's signup hooks. */
  req?: unknown;
  /** Skip the signup hooks: for identity writes that are not a signup (an import). */
  skipHooks?: boolean;
} & LoginData<Kind>;

/** The options of `IdentityStore.provision`. */
export type ProvisionOpts<Kind extends ProviderKindParam> = {
  identity?: IdentityContent;
  req?: unknown;
} & LoginData<Kind>;

/** The options of `IdentityStore.link`. `authId` names the account the identity attaches to. */
export type LinkOpts<Kind extends ProviderKindParam> = {
  authId: string;
  identity?: IdentityContent;
  req?: unknown;
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
    merge(opts: {
      fromAuthId: string;
      intoAuthId: string;
      /** The incoming request, surfaced to the app's merge function. */
      req?: unknown;
    }): Promise<void>;

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
 * What a handler's server entry produces: the handler itself, plus, for
 * handlers that own HTTP endpoints of their own (login flows, OAuth
 * callbacks, Better Auth's `/sign-in` and friends), the Node handler Wasp
 * should mount at the scheme's `mountPath`.
 *
 * One adapter returns both so they are guaranteed to share one configured
 * instance -- a handler authenticating against one configuration while its
 * routes run another is a bug class this shape makes unrepresentable.
 */
export type ServerAuthHandlerParts = {
  handler: AuthHandler;

  /**
   * Node-style request handler for the scheme's own routes, mounted at
   * `/auth/<scheme>` with the app's usual middleware around it (minus the JSON
   * body parser when the manifest asked for raw bodies). Paths the handler
   * sees are relative to the mount.
   */
  routeHandler?: (
    req: IncomingMessage,
    res: ServerResponse,
  ) => void | Promise<void>;
};

/**
 * The required shape of a handler's server half: the function Wasp calls to
 * build it. A handler package exports it (as `createServerAuthHandler`, or
 * under the name its manifest gives); a hand-written handler references it
 * from `main.wasp.ts`. The two are the same thing in different places.
 *
 * `spec` is the manifest's `server.spec`: the part of the app's Wasp Spec
 * that is this handler's own, exactly as the handler's spec constructor built it.
 * One object mixing plain data with the app's functions, each where it
 * naturally belongs. Wasp carried the functions across the compiler as
 * references and set them back, so they arrive live and callable. Wasp never
 * reads the contents; the handler types them with `ServerSpec`.
 *
 * `ProviderNames` is the union of the manifest's `providers` names.
 *
 * This is the loose form, typed by hand, for hand-written handlers. A package
 * with a spec constructor uses `ServerAuthAdapterFor<typeof myAuth>`, which derives
 * all of this, and more, from the manifest the constructor returns.
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
};
