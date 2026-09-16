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
export declare const authProviderIds: readonly ["external:wasp-auth"];
export type AuthProviderId = (typeof authProviderIds)[number];
/**
 * The provider ids a credential can be exchanged with (`POST
 * /auth/login/:providerId`): every provider except Wasp's own auth, which
 * mints sessions through its own routes. `never` when the app has no external
 * providers.
 */
export type ExternalAuthProviderId = Exclude<AuthProviderId, "wasp">;
/**
 * The capabilities each provider declared, keyed by provider id. An open set:
 * adapters may declare capabilities newer than this version of Wasp knows
 * about.
 */
export declare const authCapabilities: {
    readonly [Id in AuthProviderId]: readonly string[];
};
//# sourceMappingURL=provider.d.ts.map