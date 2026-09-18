import { ClerkProvider, useClerk } from "@clerk/clerk-react";
import { useEffect, type ReactNode } from "react";
import type { ClientAuthHandlerFactory } from "wasp/client/auth/types";

/**
 * Clerk's client half, hand-written in the app. A `ClientAuthHandlerFactory`, the
 * function a handler package exports as `createClientAuthHandler`, so Wasp wires
 * it exactly like the packaged handler in `../clerk`:
 *
 * - `Wrapper` mounts Clerk's React context around the app, without taking
 *   the app's own `client.rootComponent` slot.
 * - `getCredential` hands Wasp Clerk's token at request time, fresh across
 *   Clerk's ~60s rotations.
 * - `onCredentialChange` refreshes cached queries after a login or logout
 *   that happened inside Clerk's own widget.
 * - `onLogout` signs out of Clerk when the app calls Wasp's `logout()`.
 *
 * The publishable key arrives as `runtime.env`, because the manifest declares
 * it under `env.client`; the app needs no env schema of its own for it.
 */

// The subset of the loaded clerk-js instance this file needs.
type ClerkInstanceLike = {
  loaded: boolean;
  session?: { getToken(): Promise<string | null> } | null;
  addListener(listener: () => void): () => void;
  signOut(): Promise<void>;
};

// The channel between the React tree (where Clerk boots) and the non-React
// handler methods (which Wasp's API client calls). `getCredential` resolves
// only once Clerk is loaded, so the first authenticated request cannot race
// provider startup.
let resolveClerkInstance: (clerk: ClerkInstanceLike) => void;
const clerkInstance: Promise<ClerkInstanceLike> = new Promise((resolve) => {
  resolveClerkInstance = resolve;
});
const credentialListeners = new Set<() => void>();

function ClerkInstanceCapture({ children }: { children: ReactNode }) {
  const clerk = useClerk() as unknown as ClerkInstanceLike;

  useEffect(() => {
    if (clerk.loaded) {
      resolveClerkInstance(clerk);
    }
  }, [clerk, clerk.loaded]);

  useEffect(
    () =>
      clerk.addListener(() => {
        credentialListeners.forEach((listener) => listener());
      }),
    [clerk],
  );

  return <>{children}</>;
}

export const createClerkClientAuthHandler: ClientAuthHandlerFactory = (
  runtime,
) => ({
  Wrapper: ({ children }) => (
    <ClerkProvider
      publishableKey={runtime.env.REACT_APP_CLERK_PUBLISHABLE_KEY ?? ""}
      afterSignOutUrl="/login"
    >
      <ClerkInstanceCapture>{children}</ClerkInstanceCapture>
    </ClerkProvider>
  ),

  async getCredential() {
    const clerk = await clerkInstance;
    return (await clerk.session?.getToken()) ?? null;
  },

  onCredentialChange(listener) {
    credentialListeners.add(listener);
    return () => {
      credentialListeners.delete(listener);
    };
  },

  async onLogout() {
    const clerk = await clerkInstance;
    await clerk.signOut();
  },
});
