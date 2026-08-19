/**
 * The contract between Wasp and an authentication provider.
 *
 * Wasp builds everything users experience on top of this interface -- `authRequired`
 * pages, `auth: true` operations, `context.user`, `useAuth()` -- so implementing it
 * is all it takes to make any auth solution (Better Auth, Clerk, WorkOS, ...) a
 * Wasp auth provider.
 */

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
