{{={= =}=}}
// PUBLIC API
/**
 * The auth provider this app runs on, as a literal the type system narrows.
 *
 * Guard provider-specific code with it and TypeScript will tell you at compile
 * time when the app switches providers:
 *
 * ```ts
 * import { authProviderId } from 'wasp/auth/provider'
 * ```
 */
export const authProviderId = "{= providerId =}" as const;

// PUBLIC API
export type AuthProviderId = typeof authProviderId;

// PUBLIC API
/**
 * The capabilities the provider declared. An open set: adapters may declare
 * capabilities newer than this version of Wasp knows about.
 */
export const authCapabilities = {=& capabilities =} as const;
