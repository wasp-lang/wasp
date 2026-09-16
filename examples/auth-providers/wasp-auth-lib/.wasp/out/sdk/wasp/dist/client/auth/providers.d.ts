import type { ClientAuthAdapter } from '@wasp.sh/auth-contract/client';
import type { ExternalAuthProviderId } from '../../auth/provider.js';
export declare const clientAuthAdapters: Partial<Record<ExternalAuthProviderId, ClientAuthAdapter>>;
/**
 * Attempts to silently re-establish a Wasp session from the provider of the
 * last login in this browser (ASP.NET's silent challenge, done in-page).
 *
 * Consults exactly ONE adapter -- the one recorded at the last session mint --
 * and never probes the others, so two live provider credentials can never
 * race, and an explicit logout (which clears the marker) can never be undone
 * by resume. Runs at the auth gate (`createAuthRequiredPage`) when an
 * authRequired page finds no user; login pages that want the
 * instant-completion path may call it eagerly too.
 *
 * Resolves to whether a session now exists. Single-flighted: concurrent
 * callers share one attempt.
 */
export declare function resumeSession(): Promise<boolean>;
/**
 * Logs in through the named provider's client adapter: pulls its current
 * credential and exchanges it for a Wasp session. The provider's own sign-in
 * flow must have completed first (or its credential must otherwise be live).
 * For providers without a client adapter, obtain the credential yourself and
 * call `exchangeCredentialForSession`.
 */
export declare function loginWithAuthProvider(providerId: ExternalAuthProviderId): Promise<void>;
//# sourceMappingURL=providers.d.ts.map