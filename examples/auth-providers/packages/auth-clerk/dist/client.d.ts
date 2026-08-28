import { type ReactNode } from "react";
import type { ClientAdapterFactory } from "@wasp.sh/auth-contract/client";
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
 * via `exchangeCredentialForSession("external:clerk", token)` from
 * `wasp/client/api`.
 */
export declare function ClerkAuthProvider({ publishableKey, afterSignOutUrl, children, }: {
    publishableKey?: string;
    afterSignOutUrl?: string;
    children: ReactNode;
}): import("react").JSX.Element;
export * from "@clerk/clerk-react";
/**
 * The client half of the adapter, instantiated by Wasp's generated client.
 *
 * With this in place the app composes nothing by hand: Wasp mounts the
 * `Wrapper` around the app, pulls the current token at each request (fresh
 * across Clerk's ~60s rotations), refreshes on Clerk-side logins/logouts, and
 * `logout()` signs out of Clerk too.
 */
export declare const createClientAdapter: ClientAdapterFactory;
