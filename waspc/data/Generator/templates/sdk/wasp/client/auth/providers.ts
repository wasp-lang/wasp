{{={= =}=}}
import type { ClientAuthAdapter } from '@wasp.sh/auth-contract/client'
import type { ExternalAuthProviderId } from '../../auth/provider.js'
import {
  api,
  getLastAuthProviderId,
  getSessionId,
  removeLocalUserData,
} from '../../api/index.js'
{=# anyClientAdapters =}
import { exchangeCredentialForSession } from '../../api/index.js'
import { invalidateAndRemoveQueries } from '../operations/internal/resources.js'
import { config } from '../config.js'
import { env } from '../env.js'
{=# clientAdapterProviders =}
import { createClientAdapter as createClientAdapter_{= index =} } from '{= clientPackage =}'
{=/ clientAdapterProviders =}
{=/ anyClientAdapters =}

/**
 * The client halves of the app's auth providers, instantiated from each
 * adapter package's client entry with the same runtime-window discipline as
 * the server halves: an adapter sees only what Wasp hands it here. Keyed by
 * provider id; providers without a client package simply have no entry.
 */

{=# anyClientAdapters =}
// Each adapter's env is narrowed to exactly the vars its manifest declared:
// what an adapter reads is what its manifest shows.
function makeClientRuntime(declaredClientEnvVarNames: readonly string[]) {
  return {
    apiUrl: config.apiUrl,
    env: Object.fromEntries(
      declaredClientEnvVarNames.map((name) => [
        name,
        (env as Record<string, string | undefined>)[name],
      ]),
    ),
  }
}
{=/ anyClientAdapters =}

// PRIVATE API
export const clientAuthAdapters: Partial<Record<ExternalAuthProviderId, ClientAuthAdapter>> = {
  {=# clientAdapterProviders =}
  '{= providerId =}': createClientAdapter_{= index =}(makeClientRuntime({=& clientEnvVarNamesJs =}), {=# hasOptions =}{=& optionsJson =}{=/ hasOptions =}{=^ hasOptions =}undefined{=/ hasOptions =}),
  {=/ clientAdapterProviders =}
}

// PUBLIC API
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
export function resumeSession(): Promise<boolean> {
  {=# anyClientAdapters =}
  resumeInFlight ??= attemptResumeSession().finally(() => {
    resumeInFlight = null
  })
  return resumeInFlight
  {=/ anyClientAdapters =}
  {=^ anyClientAdapters =}
  // No provider brings a client-side credential source, so there is nothing
  // to resume from; a live session is the only way to be authenticated.
  return Promise.resolve(getSessionId() !== null)
  {=/ anyClientAdapters =}
}

{=# anyClientAdapters =}
let resumeInFlight: Promise<boolean> | null = null

async function attemptResumeSession(): Promise<boolean> {
  if (getSessionId() !== null) {
    return true
  }
  const lastProviderId = getLastAuthProviderId()
  if (lastProviderId === null) {
    return false
  }
  const adapter = clientAuthAdapters[lastProviderId as ExternalAuthProviderId]
  if (adapter?.getCredential === undefined) {
    // 'wasp' or an adapter-less provider: nothing exists outside the Wasp
    // session itself, so an expired session correctly means "log in again".
    return false
  }
  const credential = await adapter.getCredential()
  if (credential === null) {
    return false
  }
  try {
    await exchangeCredentialForSession(lastProviderId as ExternalAuthProviderId, credential)
  } catch {
    return false
  }
  await invalidateAndRemoveQueries()
  return true
}
{=/ anyClientAdapters =}

// PUBLIC API
/**
 * Logs in through the named provider's client adapter: pulls its current
 * credential and exchanges it for a Wasp session. The provider's own sign-in
 * flow must have completed first (or its credential must otherwise be live).
 * For providers without a client adapter, obtain the credential yourself and
 * call `exchangeCredentialForSession`.
 */
export async function loginWithAuthProvider(providerId: ExternalAuthProviderId): Promise<void> {
  {=# anyClientAdapters =}
  const adapter = clientAuthAdapters[providerId]
  if (adapter?.getCredential === undefined) {
    throw new Error(
      `Auth provider '${providerId}' has no client-side credential source; ` +
        `obtain the credential yourself and call exchangeCredentialForSession().`,
    )
  }
  const credential = await adapter.getCredential()
  if (credential === null) {
    throw new Error(
      `Auth provider '${providerId}' has no active credential; ` +
        `complete the provider's own sign-in flow first.`,
    )
  }
  await exchangeCredentialForSession(providerId, credential)
  await invalidateAndRemoveQueries()
  {=/ anyClientAdapters =}
  {=^ anyClientAdapters =}
  throw new Error(
    `Auth provider '${providerId}' has no client-side credential source; ` +
      `obtain the credential yourself and call exchangeCredentialForSession().`,
  )
  {=/ anyClientAdapters =}
}

{=# anyClientAdapters =}
/**
 * Credential-event wiring, per adapter. Two jobs, both attributed to the
 * emitting adapter -- nothing here ever guesses across providers:
 *
 * 1. Fresh sign-in: a null -> non-null credential transition observed after
 *    startup means the user just completed THIS provider's own UI flow, so it
 *    is exchanged for a Wasp session -- but only when no session exists (a
 *    live session is never silently replaced; log out first).
 *    A credential that already exists at startup is baseline state, not a
 *    sign-in, and is deliberately NOT exchanged: adopting it ambiently is how
 *    a lingering widget session would silently log someone in as a different
 *    account. Resume for the *last-used* provider is `resumeSession`'s job.
 * 2. Provider-side sign-out: ends the Wasp session only when THIS provider
 *    minted it (Clerk signing out must not kill a Better Auth session).
 */
for (const [providerId, adapter] of Object.entries(clientAuthAdapters)) {
  wireAdapterCredentialEvents(providerId as ExternalAuthProviderId, adapter as ClientAuthAdapter)
}

function wireAdapterCredentialEvents(
  providerId: ExternalAuthProviderId,
  adapter: ClientAuthAdapter,
): void {
  const getCredential = adapter.getCredential?.bind(adapter)
  if (getCredential === undefined || adapter.onCredentialChange === undefined) {
    return
  }

  // Baseline snapshot; `undefined` = not yet known. Only transitions observed
  // relative to a KNOWN null baseline count as fresh sign-ins.
  let lastKnownCredential: string | null | undefined = undefined
  void getCredential().then((credential) => {
    if (lastKnownCredential === undefined) {
      lastKnownCredential = credential
    }
  })

  adapter.onCredentialChange(() => {
    void handleCredentialChange()
  })

  async function handleCredentialChange(): Promise<void> {
    const credential = await getCredential!()
    const previous = lastKnownCredential
    lastKnownCredential = credential

    if (credential === null) {
      if (getSessionId() !== null && getLastAuthProviderId() === providerId) {
        try {
          await api.post('/auth/logout')
        } catch {
          // Best-effort: the session row expires on its own if the server is
          // unreachable; locally the user is logged out either way.
        }
        removeLocalUserData()
        await invalidateAndRemoveQueries()
      }
      return
    }

    if (previous === null && getSessionId() === null) {
      try {
        await exchangeCredentialForSession(providerId, credential)
        await invalidateAndRemoveQueries()
      } catch {
        // The provider's UI flow completed but the exchange failed; the login
        // page surface handles retries explicitly.
      }
    }
  }
}
{=/ anyClientAdapters =}
