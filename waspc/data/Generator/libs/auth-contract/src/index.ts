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
 * client-side counterpart lives in `@wasp.sh/auth-contract/client`.
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
   * Wasp uses it for two things only: terminating the session on logout, and
   * diagnostics. Wasp never interprets its contents.
   */
  sessionId: string;

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
 * What Wasp's internals need from an authentication provider.
 *
 * This is the whole contract for the *session read path*: the request middleware,
 * websocket authentication, and logout.
 *
 * Establishing a session (login, signup) is deliberately NOT part of this
 * interface. Providers differ irreconcilably there: Clerk, for instance, has no
 * server-side password login at all. Those capabilities belong in separate,
 * optional interfaces so that Wasp can tell which features a given provider
 * supports.
 */
export interface AuthProvider {
  /**
   * Stable identifier for this provider, e.g. `'clerk'`.
   *
   * Identities Wasp provisions for this provider's subjects are recorded under
   * this name, so it must stay stable across deploys and package versions.
   */
  readonly id: string;

  /**
   * Verify an incoming request.
   *
   * Returns `null` when the request carries no valid credential. That is *not* an
   * error -- Wasp lets unauthenticated requests through and leaves it to individual
   * operations to decide whether they require a user.
   *
   * The request is a standard web `Request`. For plain HTTP traffic Wasp builds it
   * from the incoming request, headers and all. For websocket authentication Wasp
   * synthesizes one carrying only an `Authorization: Bearer <credential>` header --
   * so an adapter must be able to authenticate from headers alone and must not
   * rely on cookies, the URL or the method being meaningful.
   */
  authenticate(request: Request): Promise<VerifiedSession | null>;

  /** Terminate a single session. */
  revokeSession(sessionId: string): Promise<void>;
}

/**
 * A provider that can also mint sessions server-side.
 *
 * In-process providers (Wasp's own auth, Better Auth) can do this. Hosted ones may
 * not: Clerk's password verification lives on its Frontend API behind a
 * browser-held cookie, so there is no server-side call that turns credentials into
 * a session. Wasp's own login routes and auth UI require this capability.
 */
export interface SessionIssuingAuthProvider extends AuthProvider {
  /** Create a new session for a subject. */
  issueSession(subjectId: string): Promise<VerifiedSession>;

  /** Terminate every session belonging to a subject. */
  revokeAllSessions(subjectId: string): Promise<void>;
}

/** Runtime capability check, mirroring the type-level distinction above. */
export function canIssueSessions(
  provider: AuthProvider,
): provider is SessionIssuingAuthProvider {
  return (
    typeof (provider as SessionIssuingAuthProvider).issueSession === "function"
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
 * `extendServerConfig` is the escape hatch for everything a manifest cannot
 * carry: functions, class instances, live values. An adapter that supports it
 * builds its default configuration, hands it to this function, and uses the
 * result -- so the user can reach the underlying library's full surface
 * (Better Auth's hooks, plugins and email callbacks, say) without giving up
 * the packaged adapter. Adapters should re-assert the invariants their
 * integration depends on (route base paths, required plugins, table name
 * overrides) after applying it.
 */
export type ServerAdapterExtensions = {
  extendServerConfig?: (config: never) => unknown;
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
