/**
 * The client-side half of the auth contract.
 *
 * An handler package with client-side needs (a React context to mount, a
 * credential to attach to requests) implements this in its client entry and
 * exposes it as a named `createClientAuthHandler` export (see
 * {@link ClientAuthHandlerFactory}). Wasp instantiates it once per scheme and
 * wires the pieces into the generated client; the app composes nothing by
 * hand.
 */

import type { ComponentType, ReactNode } from "react";

/**
 * Everything Wasp hands a client-side auth handler about the app it runs in. Like
 * its server counterpart, this is the handler's only window into the app.
 */
export type WaspClientRuntime = {
  /** The name of this scheme, as declared in the app's `auth.schemes`. */
  scheme: string;

  /** The URL the Wasp server is reachable at. */
  apiUrl: string;

  /** Where this scheme's routes are mounted: `${apiUrl}/auth/<scheme>`. */
  mountUrl: string;

  /**
   * The client-side environment, already validated against the env vars the
   * handler's manifest declared.
   */
  env: Record<string, string | undefined>;

  /**
   * Adopt a bearer credential this scheme obtained through its own routes (a
   * token its issuer minted), or drop it with `null`.
   *
   * Pre-bound to this scheme: adopting records the scheme for logout routing,
   * so a handler cannot misdirect sign-out to another scheme. Also refreshes
   * the client's cached queries, so the UI reflects the new user immediately.
   * Cookie-carried credentials never go through here; the browser holds them.
   *
   * `persistent: false` keeps the credential for the browser session only
   * (the sign-in was made without "remember me").
   */
  setCredential(
    credential: string | null,
    options?: { persistent?: boolean },
  ): Promise<void>;
} & WaspClientRuntimeRequests;

export type WaspClientRuntimeRequests = {
  /**
   * `fetch`, with the app's current auth credential attached the way Wasp's
   * own API client attaches it, for calling this scheme's own routes as the
   * signed-in user. The handler never sees the credential, and the transport
   * (bearer token or cookie) is not its concern.
   *
   * Restricted to URLs under {@link WaspClientRuntime.mountUrl}: a request
   * anywhere else is rejected, so a handler cannot spend the user's
   * credential against other routes or origins.
   */
  fetch(input: string | URL, init?: RequestInit): Promise<Response>;

  /**
   * Refetch the current user (`useAuth()`), after a change that is neither a
   * sign-in nor a sign-out: a linked account, a changed profile claim. Other
   * cached queries are left alone.
   */
  refreshUser(): Promise<void>;
};

export type ClientAuthHandler = {
  /**
   * Component Wasp composes around the app's tree, so every page renders
   * inside it. This is where a provider's React context lives (Clerk's
   * `ClerkProvider`, for one). It does not occupy the app's own
   * `rootComponent` slot.
   */
  Wrapper?: ComponentType<{ children: ReactNode }>;

  /**
   * The scheme's current bearer credential, or `null` when there is none.
   *
   * Pull-based on purpose: Wasp asks at the moment it needs the credential
   * rather than caching a pushed value, so a token that rotates under the
   * handler (short-lived JWTs) is always fresh at request time.
   * Implementations should resolve only once the provider's client is loaded.
   *
   * Optional: a handler without it is legal and simply has no credential of
   * its own to attach; the framework attaches the credential of the default
   * scheme instead.
   */
  getCredential?(): Promise<string | null>;

  /**
   * Subscribe to credential changes; returns an unsubscribe function.
   *
   * Wasp uses this to refresh the current user and re-authenticate live
   * websocket connections after a login or logout that happened outside
   * Wasp's own code -- inside a provider's sign-in component, for one.
   */
  onCredentialChange?(listener: () => void): () => void;

  /**
   * Called by Wasp's `logout()` before it signs out server-side: the
   * handler's chance to clear its own client-side state (Clerk's
   * `signOut()`, a token store's `clear()`).
   */
  onLogout?(): Promise<void>;
};

/**
 * The required shape of an handler package's client entry: a named
 * `createClientAuthHandler` export of this type. `options` is the serializable
 * configuration the handler's spec helper captured in `main.wasp.ts`,
 * delivered verbatim.
 */
export type ClientAuthHandlerFactory<Options = unknown> = (
  runtime: WaspClientRuntime,
  options: Options,
) => ClientAuthHandler;
