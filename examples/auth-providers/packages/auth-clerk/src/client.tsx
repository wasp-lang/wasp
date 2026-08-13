import { ClerkProvider, useAuth as useClerkAuth } from "@clerk/clerk-react";
import { useEffect, type ReactNode } from "react";

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
 * Deliberately contains no Wasp imports -- the bridge into Wasp's client
 * credential store is a separate hook (`useClerkWaspSessionBridge`) that the
 * app wires up with its own `wasp/client/api` functions.
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

/**
 * Bridges Clerk's session into Wasp's client.
 *
 * Wasp sends its credential as `Authorization: Bearer <token>`, so we hand it
 * Clerk's token whenever Clerk has one. This is the only Wasp-specific glue on
 * the client side.
 *
 * Clerk's tokens are short-lived (~60s) and its SDK refreshes them on a timer,
 * so this effect re-runs and keeps Wasp's stored credential fresh.
 *
 * The two functions come from the app's generated `wasp/client/api` -- this
 * package cannot import generated code, so the app passes them in:
 *
 * ```tsx
 * import { clearSessionId, setSessionId } from "wasp/client/api";
 *
 * function Bridge({ children }) {
 *   useClerkWaspSessionBridge(setSessionId, clearSessionId);
 *   return <>{children}</>;
 * }
 * ```
 *
 * Must be called from a component below `ClerkAuthProvider`.
 */
export function useClerkWaspSessionBridge(
  setSessionId: (sessionId: string) => void,
  clearSessionId: () => void,
): void {
  const { isSignedIn, getToken } = useClerkAuth();

  useEffect(() => {
    let cancelled = false;
    async function sync() {
      const token = isSignedIn ? await getToken() : null;
      if (!cancelled) {
        // Wasp stores the credential it attaches to every API call. Clearing
        // it on sign-out is what makes `logout()` uniform across providers.
        if (token) {
          setSessionId(token);
        } else {
          clearSessionId();
        }
      }
    }
    void sync();
    const interval = setInterval(() => void sync(), 30_000);
    return () => {
      cancelled = true;
      clearInterval(interval);
    };
  }, [isSignedIn, getToken, setSessionId, clearSessionId]);
}

// Clerk's own components and hooks (`SignIn`, `UserButton`, `useAuth`, ...),
// re-exported so apps need only this package on the client side.
export * from "@clerk/clerk-react";
