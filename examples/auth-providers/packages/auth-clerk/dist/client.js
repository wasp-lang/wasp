import { jsx as _jsx, Fragment as _Fragment } from "react/jsx-runtime";
import { ClerkProvider, useAuth as useClerkAuth, useClerk } from "@clerk/clerk-react";
import { useEffect } from "react";
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
export function ClerkAuthProvider({ publishableKey, afterSignOutUrl = "/login", children, }) {
    const key = publishableKey ??
        import.meta.env?.REACT_APP_CLERK_PUBLISHABLE_KEY ??
        "";
    return (_jsx(ClerkProvider, { publishableKey: key, afterSignOutUrl: afterSignOutUrl, children: children }));
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
export function useClerkWaspSessionBridge(setSessionId, clearSessionId) {
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
                }
                else {
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
// The channel between the React tree (where Clerk boots) and the non-React
// adapter methods (which Wasp's generated api client calls). `getCredential`
// resolves only once Clerk is loaded, so the first authenticated request
// cannot race provider startup -- the readiness gate the contract asks for.
let resolveClerkInstance;
const clerkInstance = new Promise((resolve) => {
    resolveClerkInstance = resolve;
});
const credentialListeners = new Set();
function ClerkInstanceCapture({ children }) {
    const clerk = useClerk();
    useEffect(() => {
        if (clerk.loaded) {
            resolveClerkInstance(clerk);
        }
    }, [clerk, clerk.loaded]);
    useEffect(() => clerk.addListener(() => {
        credentialListeners.forEach((listener) => listener());
    }), [clerk]);
    return _jsx(_Fragment, { children: children });
}
/**
 * The client half of the adapter, instantiated by Wasp's generated client.
 *
 * With this in place the app composes nothing by hand: Wasp mounts the
 * `Wrapper` around the app, pulls the current token at each request (fresh
 * across Clerk's ~60s rotations), refreshes on Clerk-side logins/logouts, and
 * `logout()` signs out of Clerk too.
 */
export const createClientAdapter = (runtime) => ({
    Wrapper: ({ children }) => (_jsx(ClerkAuthProvider, { publishableKey: runtime.env.REACT_APP_CLERK_PUBLISHABLE_KEY, children: _jsx(ClerkInstanceCapture, { children: children }) })),
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
