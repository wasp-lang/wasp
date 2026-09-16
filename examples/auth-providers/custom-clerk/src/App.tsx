import { ClerkProvider, useAuth as useClerkAuth } from "@clerk/clerk-react";
import { useEffect } from "react";
import { env } from "wasp/client";
import { registerCredentialSource } from "wasp/client/api";

// Wasp's typed client env, declared in `clientEnvSchema` (see main.wasp.ts).
// Using this rather than `import.meta.env` keeps the file type-checkable.
const publishableKey = env.REACT_APP_CLERK_PUBLISHABLE_KEY;

/**
 * Bridges Clerk's login into Wasp's requests.
 *
 * Clerk's session token IS the credential: Wasp's client asks the registered
 * source for it at request time (fresh across Clerk's ~60s rotations) and
 * puts it in the `Authorization` header, and the hand-written handler
 * verifies it on the server. This is the only Wasp-specific glue on the
 * client side; the `@wasp.sh/auth-clerk` package's client adapter does the
 * same thing automatically in the `../clerk` app.
 */
function ClerkCredentialBridge({ children }: { children: React.ReactNode }) {
  const { isSignedIn, getToken } = useClerkAuth();

  useEffect(() => {
    registerCredentialSource(async () =>
      isSignedIn ? ((await getToken()) ?? null) : null,
    );
  }, [isSignedIn, getToken]);

  return <>{children}</>;
}

export function App({ children }: { children: React.ReactNode }) {
  return (
    <ClerkProvider publishableKey={publishableKey}>
      <ClerkCredentialBridge>{children}</ClerkCredentialBridge>
    </ClerkProvider>
  );
}
