// PUBLIC API
/**
 * The auth providers this app runs on, as literals the type system narrows,
 * in `main.wasp.ts` declaration order.
 *
 * Guard provider-specific code with them and TypeScript will tell you at
 * compile time when the app changes providers:
 *
 * ```ts
 * import { authProviderIds } from 'wasp/auth/provider'
 * ```
 */
export const authProviderIds = [
  "wasp",
  "clerk",
] as const;

// PUBLIC API
export type AuthProviderId = (typeof authProviderIds)[number];

// PUBLIC API
/**
 * The capabilities each provider declared, keyed by provider id. An open set:
 * adapters may declare capabilities newer than this version of Wasp knows
 * about.
 */
export const authCapabilities: { readonly [Id in AuthProviderId]: readonly string[] } = {
  "wasp": [],
  "clerk": ['session-revocation'],
};
