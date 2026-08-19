import { ClerkProvider, useAuth as useClerkAuth } from "@clerk/clerk-react";
import { useEffect } from "react";
import { env } from "wasp/client";
import { clearSessionId, setSessionId } from "wasp/client/api";

// Wasp's typed client env, declared in `clientEnvSchema` (see main.wasp.ts).
// Using this rather than `import.meta.env` keeps the file type-checkable.
const publishableKey = env.REACT_APP_CLERK_PUBLISHABLE_KEY;

/**
 * Bridges Clerk's session into Wasp's client.
 *
 * Wasp sends its credential as `Authorization: Bearer <token>`, so we hand it
 * Clerk's token whenever Clerk has one. This is the only Wasp-specific glue on
 * the client side, and it is the mirror image of what the Better Auth example's
 * login page does after a successful sign-in.
 *
 * Clerk's tokens are short-lived (~60s) and its SDK refreshes them on a timer,
 * so this effect re-runs and keeps Wasp's stored credential fresh.
 */
function ClerkToWaspSessionBridge({ children }: { children: React.ReactNode }) {
  const { isSignedIn, getToken } = useClerkAuth();

  useEffect(() => {
    let cancelled = false;
    async function sync() {
      const token = isSignedIn ? await getToken() : null;
      if (!cancelled) {
        // Wasp stores the credential it attaches to every API call. Clearing it
        // on sign-out is what makes `logout()` uniform across providers.
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
  }, [isSignedIn, getToken]);

  return <>{children}</>;
}

export function App({ children }: { children: React.ReactNode }) {
  return (
    <ClerkProvider publishableKey={publishableKey}>
      <ClerkToWaspSessionBridge>{children}</ClerkToWaspSessionBridge>
    </ClerkProvider>
  );
}
