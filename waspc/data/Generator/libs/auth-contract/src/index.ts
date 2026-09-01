/**
 * The contract between Wasp and an authentication provider.
 *
 * Wasp builds everything users experience on top of this interface -- `authRequired`
 * pages, `auth: true` operations, `context.user`, `useAuth()` -- so implementing it
 * is all it takes to make any auth solution (Better Auth, Clerk, WorkOS, ...) a
 * Wasp auth provider.
 *
 * An adapter package implements this contract in its server entry and exposes it
 * as a named `createServerAdapter` export (see `ServerAdapterFactory`). Its
 * client side, if it has one, ships as ordinary React exports on the package's
 * own `/client` entry -- Wasp generates no client glue.
 */

import type { IncomingMessage, ServerResponse } from "node:http";

export type JsonValue =
  | string
  | number
  | boolean
  | null
  | JsonValue[]
  | { [key: string]: JsonValue };

/**
 * The result of successfully verifying a credential.
 *
 * NOTE: the primitive here is *verify*, not *fetch*. A provider turns a credential
 * that arrived with the request into an identity. It is deliberately NOT
 * `findById(id)`: an external provider (Clerk, WorkOS) validates a signed token and
 * has no way to look a subject up by id on our behalf.
 */
export type VerifiedSession = {
  /**
   * Opaque, provider-owned id for this session.
   *
   * Wasp uses it for two things only: terminating the provider's session on
   * logout (dual sign-out), and diagnostics. Wasp never interprets its
   * contents.
   *
   * Optional because a stateless verifier (a plain password or JWT check) has
   * no provider session at all -- then there is nothing to revoke upstream and
   * Wasp's own session is the only one.
   */
  sessionId?: string;

  /**
   * The provider's stable id for the authenticated subject.
   *
   * For in-process providers that own Wasp's auth tables (Wasp's own auth) this is
   * the `Auth` entity's id. For external providers it is the provider's own user
   * id, and Wasp resolves it to a local user -- provisioning one on first sight.
   */
  subjectId: string;

  /**
   * Verified profile data about the subject, as far as the provider knows it:
   * email, name, avatar, whatever the verified token or session carried.
   *
   * Wasp feeds this to the app's `userSignupFields` when it provisions a local
   * user for a subject it has not seen before, and records it on the identity it
   * creates. Without it, an app whose user entity has required columns (a
   * non-nullable `email`, say) could never provision anyone.
   *
   * Adapters should document which keys they populate. Omit rather than invent:
   * an absent claim is recoverable, a made-up one is not.
   */
  claims?: Record<string, JsonValue>;
};

/**
 * The outcome of verifying a request.
 *
 * Deliberately a tagged union rather than `VerifiedSession | null`: call sites
 * read as prose, and future outcomes (an explicit "invalid credential" state,
 * say) become additive union members instead of signature breaks.
 */
export type AuthenticateResult =
  | { status: "authenticated"; session: VerifiedSession }
  | { status: "unauthenticated" };

/**
 * What Wasp's internals need from every authentication provider: the *session
 * read path* -- the request middleware and websocket authentication.
 *
 * Everything else is a capability. Establishing a session (login, signup) is
 * deliberately NOT here: providers differ irreconcilably (Clerk has no
 * server-side password login at all). Even ending a session is optional -- a
 * pure token verifier (corporate SSO, an authenticating proxy) has nothing to
 * revoke. Capabilities live in separate interfaces, and their presence IS the
 * capability: Wasp detects what a provider can do by which methods exist.
 */
export interface AuthProvider {
  /**
   * Stable identifier for this provider, e.g. `'external:clerk'`.
   *
   * External providers MUST use an `external:` prefix -- including the adapters
   * Wasp itself ships. The unprefixed namespace is reserved for Wasp's own auth
   * methods (`'email'`, `'username'`, `'google'`, ...), which record identities
   * in the same place; the prefix is what makes a collision impossible.
   *
   * Identities Wasp provisions for this provider's subjects are recorded under
   * this name, so it must stay stable across deploys and package versions.
   */
  readonly id: string;

  /**
   * Verify an incoming request.
   *
   * Returns `{ status: "unauthenticated" }` when the request carries no valid
   * credential. That is *not* an error -- Wasp lets unauthenticated requests
   * through and leaves it to individual operations to decide whether they
   * require a user.
   *
   * The request is a standard web `Request`. For plain HTTP traffic Wasp builds it
   * from the incoming request, headers and all. For websocket authentication Wasp
   * synthesizes one carrying only an `Authorization: Bearer <credential>` header --
   * so an adapter must be able to authenticate from headers alone and must not
   * rely on cookies, the URL or the method being meaningful.
   */
  authenticate(request: Request): Promise<AuthenticateResult>;
}

/**
 * Ability: end a single session server-side.
 *
 * These are pure capability mixins, not providers -- an adapter declares what it
 * is by intersection: `AuthProvider & SupportsSessionRevocation & ...`. Their
 * presence IS the capability; Wasp detects each by whether the method exists.
 *
 * Most providers support this: Wasp's own auth deletes the session row
 * (instant), Clerk revokes at the source and the current token drains out
 * within its ~60s lifetime (eventual). A pure token verifier cannot -- the
 * session lives with an identity provider the app cannot reach, and logout
 * there is the client dropping its credential. Such a provider simply omits
 * this capability, and Wasp's `logout()` promises no more than that.
 *
 * A provider whose credential rides a cookie MUST support this: without
 * revocation, a shared-computer logout would silently re-authenticate the next
 * visitor. Wasp rejects that combination at boot.
 */
export interface SupportsSessionRevocation {
  /** Terminate a single session. */
  revokeSession(sessionId: string): Promise<void>;
}

/**
 * Ability: end every session belonging to a subject.
 *
 * Deliberately independent of {@link SupportsSessionRevocation}: revoking one
 * session needs only a handle you already hold, while revoking all of them
 * needs an index by subject. Providers exist with either ability alone -- an
 * RFC 7009 token-revocation endpoint can kill only the token you present, and
 * a security-stamp scheme can kill everything at once without being able to
 * find any single session.
 */
export interface SupportsAllSessionsRevocation {
  /** Terminate every session belonging to a subject. */
  revokeAllSessions(subjectId: string): Promise<void>;
}

/**
 * Ability: mint sessions server-side.
 *
 * In-process providers (Wasp's own auth, Better Auth) can. Hosted ones may
 * not: Clerk's password verification lives on its Frontend API behind a
 * browser-held cookie, so there is no server-side call that turns credentials
 * into a session.
 */
export interface SupportsSessionIssuance {
  /** Create a new session for a subject. */
  issueSession(subjectId: string): Promise<VerifiedSession>;
}

/**
 * The full set Wasp requires of a provider that carries Wasp's OWN login and
 * signup flows (its auth routes, its forms, password reset).
 *
 * The pairing is Wasp's policy, not an implication: a stateless JWT issuer can
 * mint sessions it cannot kill, and Wasp refuses to run its login flows over
 * sessions it cannot revoke -- that is the classic unfixable-JWT-logout hole.
 * Issuing adapters should annotate with this alias, so a missing method fails
 * at compile time rather than at boot.
 */
export type SessionManagingAuthProvider = AuthProvider &
  SupportsSessionRevocation &
  SupportsAllSessionsRevocation &
  SupportsSessionIssuance;

/** Runtime capability check: can this provider end a single session? */
export function canRevokeSessions(
  provider: AuthProvider,
): provider is AuthProvider & SupportsSessionRevocation {
  return (
    typeof (provider as AuthProvider & SupportsSessionRevocation)
      .revokeSession === "function"
  );
}

/**
 * Runtime check for Wasp's session-management policy: issuing is recognized
 * only alongside full revocation (see {@link SessionManagingAuthProvider}).
 */
export function canManageSessions(
  provider: AuthProvider,
): provider is SessionManagingAuthProvider {
  const p = provider as SessionManagingAuthProvider;
  return (
    typeof p.issueSession === "function" &&
    typeof p.revokeSession === "function" &&
    typeof p.revokeAllSessions === "function"
  );
}

/**
 * Runtime facets an adapter may request from Wasp through its manifest's
 * `uses` list.
 *
 * A closed set on purpose: the generator wires only the facets it knows, so an
 * unknown name is a compile error rather than an absent property at runtime.
 * Requesting a grant is also an audit surface -- a reviewer reads `uses: [...]`
 * in the manifest and knows the adapter's blast radius.
 */
export type RuntimeGrantName =
  | "wasp-sessions"
  | "email-send"
  | "identity-namespaces";

/**
 * A reference to a subject of the calling provider.
 *
 * `namespace` must be one of the provider's declared identity namespaces
 * (default: the manifest id). The namespace-membership check is what makes
 * acting on another provider's user unrepresentable through the granted
 * facets -- the identity store itself resolves any namespace string, so the
 * guard, not the lookup, carries that guarantee.
 */
export type SubjectRef = {
  /** One of the provider's declared identity namespaces. Default: the manifest id. */
  namespace?: string;
  /** The provider's stable subject id in that namespace -- same value {@link VerifiedSession} carries. */
  subjectId: string;
};

/**
 * The `wasp-sessions` grant: mint and revoke Wasp's own sessions.
 *
 * Minting is subject-bound: `issue` resolves the subject through the calling
 * provider's OWN declared namespaces and stamps THIS provider's id on the
 * session (feeding `authRequired: [...]` provider lists) -- an adapter can
 * neither mint a session for a user it never provisioned nor attribute a
 * session to another provider. The revocation methods are fail-safe: their
 * worst misuse is a forced logout, never a login.
 */
export type WaspSessions = {
  /**
   * Mint a Wasp session for a subject this provider has provisioned.
   * Rejects a namespace outside the provider's declared set
   * (`wasp-auth/undeclared-namespace`) and an unknown subject
   * (`wasp-auth/identity-not-found`).
   *
   * Fires the app's `onBeforeLogin` (a throw vetoes the mint) and
   * `onAfterLogin` hooks -- minting through this facet is the choke point
   * that guarantees no provider skips the app's login policy.
   */
  issue(
    subject: SubjectRef,
    opts?: {
      /** The provider's own session id, stored for dual sign-out at logout. */
      providerSessionId?: string;
      /**
       * Opaque provider context surfaced to the app's login hooks as their
       * `oauth` field (OAuth tokens, typically).
       */
      hookContext?: unknown;
      /** The incoming request, surfaced to the app's login hooks. */
      req?: unknown;
      /**
       * Skip the app's login hooks for THIS mint. Only for flows that
       * already fired them at a more informative moment (an OAuth callback
       * holding tokens the later redeem step no longer has).
       */
      skipHooks?: boolean;
    },
  ): Promise<{ sessionId: string }>;

  /** Revoke a single Wasp session. */
  revoke(sessionId: string): Promise<void>;

  /**
   * Revoke every WASP session of the person behind this subject, across all
   * minting providers (the password-rotation semantic). Deliberately
   * Wasp-side only: it does NOT call into any provider's own `revokeSession`,
   * so an adapter may call it from inside its own revocation path without
   * recursion. Upstream dual sign-out stays Wasp-owned, at logout.
   */
  revokeAllForSubject(subject: SubjectRef): Promise<void>;
};

/**
 * The `email-send` grant: send through the app's configured `emailSender`.
 *
 * Requesting it is a compile-time claim -- Wasp rejects the manifest when the
 * app has no `emailSender` -- so an OTP or magic-link adapter can never ship
 * into an app that silently drops its emails. SMTP credentials never reach
 * the adapter; only the send capability does.
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
 * Codes rather than error classes on purpose: adapter packages hold their own
 * copy of this contract, and `instanceof` does not survive package-copy
 * boundaries (tsc unifies by name@version, Node does not).
 */
export type AuthContractErrorCode =
  | "wasp-auth/duplicate-identity"
  | "wasp-auth/identity-not-found"
  | "wasp-auth/undeclared-namespace"
  /**
   * The app's onBeforeSignup/onBeforeLogin hook rejected the action by
   * throwing. The thrown error itself is what carries this code (Wasp tags
   * it rather than wrapping, so its message and type survive) -- an
   * adapter's routes should map it to a 4xx carrying `error.message`, not
   * to a 500.
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
    code === "wasp-auth/undeclared-namespace" ||
    code === "wasp-auth/policy-veto"
    ? code
    : null;
}

/**
 * The facets a manifest's `uses` list grants, as types: a declared grant is a
 * non-optional member of the adapter's runtime, an undeclared one is absent.
 * An adapter annotates its factory as
 * `ServerAdapterFactory<MyOptions, "wasp-sessions" | "email-send">` and gets
 * exactly the surface its manifest claims.
 */
export type GrantedFacets<G extends RuntimeGrantName> =
  ("wasp-sessions" extends G
    ? { sessions: WaspSessions }
    : { sessions?: WaspSessions }) &
    ("email-send" extends G ? { email: WaspEmail } : { email?: WaspEmail }) &
    ("identity-namespaces" extends G
      ? { identityNamespaces: (namespace: string) => ProviderIdentities }
      : { identityNamespaces?: (namespace: string) => ProviderIdentities });

/**
 * Everything Wasp hands a server-side adapter about the app it runs in.
 *
 * This is the adapter's *only* window into the app: adapters must not import
 * generated code (`wasp/...`) and must not read `process.env` themselves. Keeping
 * the boundary here is what lets an adapter package typecheck and version
 * independently of any particular Wasp app.
 */
export type WaspServerRuntime<G extends RuntimeGrantName = never> =
  WaspServerRuntimeBase & GrantedFacets<G>;

type WaspServerRuntimeBase = {
  /**
   * The app's PrismaClient instance. Typed as `unknown` because the client's type
   * is generated per app; adapters that need it narrow it themselves.
   */
  db: unknown;

  /**
   * The Prisma datasource provider of the app's database: `"sqlite"`,
   * `"postgresql"`, ... Adapters that bring their own storage layer (Better
   * Auth's prisma adapter, for one) need to know the dialect they are talking to.
   */
  dbProvider: string;

  /**
   * The server-side environment, already validated against the env vars the
   * adapter's manifest declared.
   */
  env: Record<string, string | undefined>;

  /** The URL the Wasp server is reachable at. */
  serverUrl: string;

  /** The URL the Wasp client is served from. Useful for trusted-origin checks. */
  clientUrl: string;

  /**
   * Whether the app runs in development mode. For dev-only conveniences and
   * the `secure` flag on any cookies the adapter's routes set.
   */
  isDevelopment: boolean;

  /**
   * The identity store, pre-bound to this provider's manifest id: the
   * sanctioned channel for everything identity-shaped, with the same powers
   * Wasp's own auth flows use.
   *
   * - `provision` is the eager-provisioning channel: an in-process adapter
   *   that observes its own signup moment (Better Auth can; a hosted provider
   *   cannot) reports it here, so the local user exists from signup rather
   *   than from the first login exchange. Idempotent, and just-in-time
   *   provisioning at the exchange remains the backstop regardless.
   * - `data`/`secrets` accessors let an adapter keep per-identity state
   *   without touching `db`: non-secret working state in `data`, secret
   *   material in `secrets` -- a column the app's Prisma client omits by
   *   default, so it cannot leak through app code. Secrets are stored as
   *   given; hashing is the adapter's job.
   */
  identities: ProviderIdentities;
};

/**
 * The per-provider view of Wasp's identity store. `subjectId` is always the
 * provider's own stable user id -- the same value {@link VerifiedSession}
 * carries.
 */
export type ProviderIdentities = {
  /** The identity (claims and non-secret data), or null if never provisioned. */
  find(subjectId: string): Promise<{
    authId: string;
    claims: Record<string, JsonValue>;
    data: Record<string, JsonValue>;
  } | null>;

  /**
   * Idempotent create of the local user for a subject. Runs the app's
   * `userSignupFields` over the claims, exactly like just-in-time
   * provisioning at the login exchange.
   */
  provision(
    subjectId: string,
    identity?: {
      claims?: Record<string, JsonValue>;
      data?: Record<string, JsonValue>;
      secrets?: Record<string, JsonValue>;
    },
  ): Promise<{ authId: string } | null>;

  /**
   * Strict create of the local user for a subject: signup semantics, where
   * `provision` is login semantics. Rejects with
   * `wasp-auth/duplicate-identity` when the subject already exists.
   *
   * `getUserFields` computes the new user entity's own fields; it is a
   * callback (not a value) so the provisioning layer controls when it runs --
   * the app's signup veto, once it fires at this choke point, must run before
   * any user-supplied field getters do. When omitted, the provider's
   * manifest-level `userSignupFields` run over the claims instead.
   */
  create(
    subjectId: string,
    identity?: {
      claims?: Record<string, JsonValue>;
      data?: Record<string, JsonValue>;
      secrets?: Record<string, JsonValue>;
    },
    getUserFields?: () =>
      | Promise<Record<string, JsonValue>>
      | Record<string, JsonValue>,
    opts?: {
      /**
       * Skip the app's signup hooks for THIS create. For identity writes that
       * are not a signup (migrations, admin imports) -- the documented escape
       * hatch, so an ordinary signup can never forget the app's veto.
       */
      skipHooks?: boolean;
      /**
       * Opaque provider context surfaced to the app's `onAfterSignup` hook as
       * its `oauth` field (OAuth tokens, typically).
       */
      hookContext?: unknown;
      /** The incoming request, surfaced to the app's signup hooks. */
      req?: unknown;
    },
  ): Promise<{ authId: string }>;

  /**
   * Deletes the subject's identity AND its whole local user, cascading to auth
   * data and sessions. Returns whether anything was deleted. Loud on purpose:
   * this removes the app's business user, not just the identity row.
   */
  deleteUser(subjectId: string): Promise<boolean>;

  /** Merges the updates into the identity's non-secret data. */
  updateData(
    subjectId: string,
    updates: Record<string, JsonValue>,
  ): Promise<void>;

  /** Reads the identity's secret material. Keep the result on the server. */
  getSecrets(subjectId: string): Promise<Record<string, JsonValue> | null>;

  /** Replaces the identity's secret material. Expects it already hashed. */
  setSecrets(
    subjectId: string,
    secrets: Record<string, JsonValue>,
  ): Promise<void>;
};

/**
 * What an adapter's server entry produces: the provider itself, plus, for
 * providers that own HTTP endpoints of their own (Better Auth's `/sign-in` and
 * friends), the handler Wasp should mount at the manifest's `basePath`.
 *
 * One factory returns both so they are guaranteed to share one configured
 * instance -- a provider verifying against one configuration while its routes run
 * another is a bug class this shape makes unrepresentable.
 */
export type ServerAdapter = {
  provider: AuthProvider;

  /**
   * Node-style request handler for the provider's own routes. Wasp mounts it at
   * the `basePath` the adapter's manifest declared, with the app's usual
   * middleware around it (minus the JSON body parser when the manifest asked for
   * raw bodies).
   */
  routeHandler?: (
    req: IncomingMessage,
    res: ServerResponse,
  ) => void | Promise<void>;
};

/**
 * User-code extensions Wasp delivers alongside the serializable options.
 *
 * `setupFn` follows the same convention as Wasp's `prismaSetupFn`: a user
 * function the adapter calls with its integration configuration, whose return
 * value becomes the configuration to use. It is the escape hatch for
 * everything a manifest cannot carry -- functions, class instances, live
 * values -- so the user can reach the underlying library's full surface
 * (Better Auth's hooks, plugins and email callbacks, say) without giving up
 * the packaged adapter. Adapters should re-assert the invariants their
 * integration depends on (route base paths, required plugins, table name
 * overrides) after applying it.
 */
export type ServerAdapterExtensions = {
  setupFn?: (config: never) => unknown;
};

/**
 * The required shape of an adapter package's server entry: a named
 * `createServerAdapter` export of this type. `options` is the serializable
 * configuration the adapter's spec helper captured in `main.wasp.ts`, delivered
 * verbatim; `extensions` carries the user-code escape hatches referenced by the
 * manifest.
 */
export type ServerAdapterFactory<
  Options = unknown,
  Grants extends RuntimeGrantName = never,
> = (
  runtime: WaspServerRuntime<Grants>,
  options: Options,
  extensions?: ServerAdapterExtensions,
) => ServerAdapter | Promise<ServerAdapter>;
