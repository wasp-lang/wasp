# Auth schemes — Clerk

Wasp authenticates every request through **Clerk**, a hosted provider, via the
`@wasp.sh/auth-clerk` handler package (`../packages/auth-clerk`).

```ts
import { clerk } from "@wasp.sh/auth-clerk/spec";

auth: {
  userEntity: "User",
  onAuthFailedRedirectTo: "/login",
  schemes: { clerk: clerk() },
}
```

That one call is the whole integration: the manifest names the handler's server and client
entries and the env vars they need. The app contains no handler code of its own, no tables and
no routes.

## Needs credentials to run

Create a free application at [dashboard.clerk.com](https://dashboard.clerk.com), then:

```sh
# .env.server
CLERK_SECRET_KEY=sk_test_…                # Dashboard → API keys → Secret key
CLERK_PUBLISHABLE_KEY=pk_test_…           # Dashboard → API keys → Publishable key
CLERK_JWT_KEY=-----BEGIN PUBLIC KEY-----… # optional: verifies tokens locally, no network call

# .env.client
REACT_APP_CLERK_PUBLISHABLE_KEY=pk_test_… # same publishable key
```

## How it works

Clerk's session token **is** the credential. The package's client adapter mounts Clerk's React
context, hands the current token to Wasp at request time (fresh across Clerk's ~60s rotations),
and the server handler verifies it on every request. The scheme declares no `credentials`, so
Wasp issues nothing and adds no `Session` table. `logout()` revokes the Clerk session
server-side and signs out of Clerk on the client.

Clerk has no server-side password login (verification lives on its Frontend API behind a
browser-held cookie), which is why `signIn` is optional on the handler contract: no other
scheme can sign into Clerk, and the login page is Clerk's own `<SignIn />`.

## Run it

```sh
wasp db migrate-dev
wasp start
```
