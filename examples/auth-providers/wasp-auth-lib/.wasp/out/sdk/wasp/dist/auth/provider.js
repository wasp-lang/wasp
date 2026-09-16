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
    "external:wasp-auth",
];
// PUBLIC API
/**
 * The capabilities each provider declared, keyed by provider id. An open set:
 * adapters may declare capabilities newer than this version of Wasp knows
 * about.
 */
export const authCapabilities = {
    "external:wasp-auth": [],
};
//# sourceMappingURL=provider.js.map