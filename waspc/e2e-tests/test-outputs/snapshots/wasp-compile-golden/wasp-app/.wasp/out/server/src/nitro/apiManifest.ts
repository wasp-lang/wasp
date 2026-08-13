/**
 * The URL prefixes the Express app is allowed to answer, so that requests for
 * the app's pages never enter it.
 *
 * Custom API routes are Express route patterns (`/foo/:id`), which only Express
 * can match, so we list the part of them that is a plain prefix (`/foo`). That
 * makes this list a filter, not a router: a request matching a prefix still
 * goes through Express's own routing, and falls through to the page renderer if
 * Express has nothing for it.
 *
 * An app with a server setup function gets `/` instead: the setup function can
 * add routes of its own, and there is no way of knowing what they are.
 */
// PRIVATE API (SDK, server)
export const bridgedPathPrefixes: string[] = ['/_wasp/health', '/operations']
