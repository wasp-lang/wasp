import { ClerkProvider, useClerk } from "@clerk/clerk-react";
import { useEffect, type ReactNode } from "react";

import type {
  ClientAdapterFactory,
  ClientAuthAdapter,
} from "@wasp.sh/auth-contract/client";

/**
 * Vite exposes client env vars on `import.meta.env`; other bundlers may not
 * define it at all, so the lookup is defensive on purpose.
 */
type ViteLikeImportMeta = ImportMeta & {
  env?: Record<string, string | undefined>;
};

/**
 * Wraps the app in Clerk's React context.
 *
 * A plain `ClerkProvider` wrapper with a Wasp-friendly default: when no
 * `publishableKey` prop is given it falls back to
 * `import.meta.env.REACT_APP_CLERK_PUBLISHABLE_KEY`, the name Wasp exposes
 * client env vars under. Prefer passing the key explicitly from the app's
 * validated client env.
 *
 * Deliberately contains no Wasp imports. Apps wiring Clerk by hand (without
 * the packaged adapter) exchange Clerk's token for a Wasp session themselves
 * via `exchangeCredentialForSession("clerk", token)` from
 * `wasp/client/api`.
 */
export function ClerkAuthProvider({
  publishableKey,
  afterSignOutUrl = "/login",
  children,
}: {
  publishableKey?: string;
  afterSignOutUrl?: string;
  children: ReactNode;
}) {
  const key =
    publishableKey ??
    (import.meta as ViteLikeImportMeta).env?.REACT_APP_CLERK_PUBLISHABLE_KEY ??
    "";

  return (
    <ClerkProvider publishableKey={key} afterSignOutUrl={afterSignOutUrl}>
      {children}
    </ClerkProvider>
  );
}

// Clerk's own components and hooks (`SignIn`, `UserButton`, `useAuth`, ...),
// re-exported so apps need only this package on the client side.
export * from "@clerk/clerk-react";

/**
 * The subset of the loaded clerk-js instance the adapter needs. Typed locally
 * so this module does not depend on clerk-js internals.
 */
type ClerkInstanceLike = {
  loaded: boolean;
  session?: { getToken(): Promise<string | null> } | null;
  addListener(listener: () => void): () => void;
  signOut(): Promise<void>;
};

// The channel between the React tree (where Clerk boots) and the non-React
// adapter methods (which Wasp's generated api client calls). `getCredential`
// resolves only once Clerk is loaded, so the first authenticated request
// cannot race provider startup -- the readiness gate the contract asks for.
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

/**
 * The client half of the adapter, instantiated by Wasp's generated client.
 *
 * With this in place the app composes nothing by hand: Wasp mounts the
 * `Wrapper` around the app, pulls the current token at each request (fresh
 * across Clerk's ~60s rotations), refreshes on Clerk-side logins/logouts, and
 * `logout()` signs out of Clerk too.
 */
export const createClientAdapter: ClientAdapterFactory = (
  runtime,
): ClientAuthAdapter => ({
  Wrapper: ({ children }) => (
    <ClerkAuthProvider
      publishableKey={runtime.env.REACT_APP_CLERK_PUBLISHABLE_KEY}
    >
      <ClerkInstanceCapture>{children}</ClerkInstanceCapture>
    </ClerkAuthProvider>
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
