import { ClerkProvider, useAuth as useClerkAuth } from "@clerk/clerk-react";
import { useEffect } from "react";
import { env } from "wasp/client";
import {
  clearSessionId,
  exchangeCredentialForSession,
  getSessionId,
} from "wasp/client/api";

// Wasp's typed client env, declared in `clientEnvSchema` (see main.wasp.ts).
// Using this rather than `import.meta.env` keeps the file type-checkable.
const publishableKey = env.REACT_APP_CLERK_PUBLISHABLE_KEY;

/**
 * Bridges Clerk's login into Wasp's session.
 *
 * Wasp mints its own session: after Clerk signs the user in, we exchange
 * Clerk's token for a Wasp session once (`POST /auth/login`), and every
 * subsequent request authenticates against Wasp -- Clerk is off the request
 * path until logout. This is the only Wasp-specific glue on the client side;
 * the `@wasp.sh/auth-clerk` package's client adapter does the same thing
 * automatically in the `../clerk` app.
 *
 * The interval covers two recoveries: a Wasp session that got cleared by a 401
 * is re-exchanged while Clerk is still signed in, and a Clerk sign-out drops
 * the local session state.
 */
function ClerkToWaspSessionBridge({ children }: { children: React.ReactNode }) {
  const { isSignedIn, getToken } = useClerkAuth();

  useEffect(() => {
    let cancelled = false;
    async function sync() {
      if (cancelled) return;
      if (isSignedIn && getSessionId() === null) {
        const token = await getToken();
        if (token && !cancelled) {
          try {
            await exchangeCredentialForSession("clerk", token);
          } catch {
            // The next tick retries; a misconfigured server keeps failing
            // loudly in its own logs.
          }
        }
      } else if (!isSignedIn && getSessionId() !== null) {
        clearSessionId();
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
