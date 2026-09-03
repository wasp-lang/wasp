import { jsx as _jsx, Fragment as _Fragment } from "react/jsx-runtime";
import { ClerkProvider, useClerk } from "@clerk/clerk-react";
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
 * Deliberately contains no Wasp imports. Apps wiring Clerk by hand (without
 * the packaged adapter) exchange Clerk's token for a Wasp session themselves
 * via `exchangeCredentialForSession("clerk", token)` from
 * `wasp/client/api`.
 */
export function ClerkAuthProvider({ publishableKey, afterSignOutUrl = "/login", children, }) {
    const key = publishableKey ??
        import.meta.env?.REACT_APP_CLERK_PUBLISHABLE_KEY ??
        "";
    return (_jsx(ClerkProvider, { publishableKey: key, afterSignOutUrl: afterSignOutUrl, children: children }));
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
