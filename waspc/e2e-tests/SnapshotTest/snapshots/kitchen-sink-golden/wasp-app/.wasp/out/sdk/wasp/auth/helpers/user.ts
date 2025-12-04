import { setSessionId } from 'wasp/client/api'
import { invalidateAndRemoveQueries } from '../../client/operations/internal/resources.js'

// PRIVATE API
export async function initSession(sessionId: string): Promise<void> {
    setSessionId(sessionId)
    // We need to invalidate queries after login in order to get the correct user
    // data in the React components (using `useAuth`).
    // Redirects after login won't work properly without this.

    // TODO(filip): We are currently removing all the queries, but we should
    // remove only non-public, user-dependent queries - public queries are
    // expected not to change in respect to the currently logged in user.
    await invalidateAndRemoveQueries()
}
