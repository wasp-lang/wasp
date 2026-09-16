// PUBLIC API
/**
 * The auth schemes this app runs on, as literals the type system narrows,
 * in `main.wasp.ts` declaration order.
 *
 * Guard scheme-specific code with them and TypeScript will tell you at
 * compile time when the app changes schemes:
 *
 * ```ts
 * import { authSchemeNames } from 'wasp/auth/scheme'
 * ```
 */
export const authSchemeNames = [
  "clerk",
] as const;

// PUBLIC API
export type AuthSchemeName = (typeof authSchemeNames)[number];

// PUBLIC API
/**
 * The scheme that authenticates assets which only say `authRequired: true`.
 */
export const defaultAuthScheme: AuthSchemeName = "clerk";

// PUBLIC API
/**
 * The capabilities each scheme's handler declared, keyed by scheme name. An
 * open set: handlers may declare capabilities newer than this version of
 * Wasp knows about.
 */
export const authCapabilities: { readonly [Name in AuthSchemeName]: readonly string[] } = {
  "clerk": [],
};
