import {
  ClerkAuthProvider,
  useClerkWaspSessionBridge,
} from "@wasp.sh/auth-clerk/client";
import { env } from "wasp/client";
import { clearSessionId, setSessionId } from "wasp/client/api";

/**
 * The bridge logic lives in the adapter package; the app only supplies the
 * two functions from its generated `wasp/client/api` -- the package cannot
 * import generated code, so this composition is the whole client-side wiring.
 */
function ClerkToWaspSessionBridge({ children }: { children: React.ReactNode }) {
  useClerkWaspSessionBridge(setSessionId, clearSessionId);
  return <>{children}</>;
}

export function App({ children }: { children: React.ReactNode }) {
  return (
    // Wasp's typed client env, declared in `clientEnvSchema` (see
    // main.wasp.ts). Using this rather than `import.meta.env` keeps the file
    // type-checkable.
    <ClerkAuthProvider publishableKey={env.REACT_APP_CLERK_PUBLISHABLE_KEY}>
      <ClerkToWaspSessionBridge>{children}</ClerkToWaspSessionBridge>
    </ClerkAuthProvider>
  );
}
