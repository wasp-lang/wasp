/**
 * The client-side half of the auth provider contract.
 *
 * An adapter package that has client-side needs (a context provider to mount, a
 * token to attach to requests) implements this in its client entry and exposes it
 * as a named `createClientAdapter` export (see `ClientAdapterFactory`).
 */

import type { ComponentType, ReactNode } from "react";

/**
 * Everything Wasp hands a client-side adapter about the app it runs in. Like its
 * server counterpart, this is the adapter's only window into the app.
 */
export type WaspClientRuntime = {
  /** The URL the Wasp server is reachable at. */
  apiUrl: string;

  /**
   * The client-side environment, already validated against the env vars the
   * adapter's manifest declared.
   */
  env: Record<string, string | undefined>;
};

export type ClientAuthAdapter = {
  /**
   * Component Wasp composes around the router, so every page renders inside it.
   * This is where a provider's React context lives (Clerk's `ClerkProvider`, for
   * one). It does not occupy the app's own `rootComponent` slot.
   */
  Wrapper?: ComponentType<{ children: ReactNode }>;

  /**
   * The credential to attach to the next request, or `null` when there is none.
   *
   * Pull-based on purpose: Wasp asks at each request rather than caching a
   * pushed value, so a token that rotates under the adapter (short-lived JWTs)
   * is always current.
   */
  getCredential(): Promise<string | null>;

  /**
   * Subscribe to credential changes; returns an unsubscribe function.
   *
   * Wasp uses this to re-authenticate live websocket connections and to refresh
   * the current user after a login or logout that happened outside Wasp's own
   * code -- e.g. inside a provider's sign-in component. An adapter that never
   * changes credentials without a full page load can omit it.
   */
  onCredentialChange?(listener: () => void): () => void;

  /**
   * Called by Wasp's `logout()` before it revokes the session server-side. This
   * is the adapter's chance to clear its own client-side state (Clerk's
   * `signOut()`, a token store's `clear()`).
   */
  onLogout?(): Promise<void>;
};

/**
 * The required shape of an adapter package's client entry: a named
 * `createClientAdapter` export of this type. `options` is the serializable
 * configuration the adapter's spec helper captured in `main.wasp.ts`, delivered
 * verbatim.
 */
export type ClientAdapterFactory<Options = unknown> = (
  runtime: WaspClientRuntime,
  options: Options,
) => ClientAuthAdapter;
