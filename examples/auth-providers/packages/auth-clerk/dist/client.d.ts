import { type ReactNode } from "react";
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
export declare function ClerkAuthProvider({ publishableKey, afterSignOutUrl, children, }: {
    publishableKey?: string;
    afterSignOutUrl?: string;
    children: ReactNode;
}): import("react").JSX.Element;
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
export declare function useClerkWaspSessionBridge(setSessionId: (sessionId: string) => void, clearSessionId: () => void): void;
export * from "@clerk/clerk-react";
