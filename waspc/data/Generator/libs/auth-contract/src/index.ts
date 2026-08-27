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
 * Everything Wasp hands a server-side adapter about the app it runs in.
 *
 * This is the adapter's *only* window into the app: adapters must not import
 * generated code (`wasp/...`) and must not read `process.env` themselves. Keeping
 * the boundary here is what lets an adapter package typecheck and version
 * independently of any particular Wasp app.
 */
export type WaspServerRuntime = {
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
export type ServerAdapterFactory<Options = unknown> = (
  runtime: WaspServerRuntime,
  options: Options,
  extensions?: ServerAdapterExtensions,
) => ServerAdapter | Promise<ServerAdapter>;
