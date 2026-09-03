# Auth providers — Clerk

Wasp authenticates every request through **Clerk**, a hosted provider, via the
`@wasp.sh/auth-clerk` adapter package (`../packages/auth-clerk`).

```ts
import { clerk } from "@wasp.sh/auth-clerk/spec";

auth: {
  userEntity: "User",
  onAuthFailedRedirectTo: "/login",
  provider: clerk(), // Clerk verifies instead of Wasp's own auth
}
```

That one call is the whole server-side integration: the manifest it produces
names the adapter's server entry, its capabilities and the env vars it needs.
The app contains no adapter code of its own.

## ⚠️ Needs credentials to run

This app compiles and boots with placeholder keys, but cannot authenticate anyone without a real
Clerk instance. **You need to supply four values.**

Create a free application at [dashboard.clerk.com](https://dashboard.clerk.com), then:

```sh
# .env.server
CLERK_SECRET_KEY=sk_test_…                # Dashboard → API keys → Secret key
CLERK_PUBLISHABLE_KEY=pk_test_…           # Dashboard → API keys → Publishable key
CLERK_JWT_KEY=-----BEGIN PUBLIC KEY-----… # optional, see below
JWT_SECRET=example-app-development-secret-0123456789abcdef

# .env.client
REACT_APP_CLERK_PUBLISHABLE_KEY=pk_test_… # same publishable key
```

`CLERK_JWT_KEY` is optional but worth setting: with it, `authenticateRequest` verifies the token
locally with **no network call per request**. Without it, Clerk fetches and caches the JWKS.
Dashboard → API keys → Show JWT public key → PEM.

### On free tiers

**Clerk** is the one used here: **50,000 monthly active users free**, no credit card. Plenty for
this.

**WorkOS AuthKit** is the alternative worth knowing about — **1,000,000 MAU free**, and unlike
Clerk it _does_ expose server-side password auth (`authenticateWithPassword`) and genuinely
blocking registration hooks. So a WorkOS adapter could add
`SupportsSessionIssuance` and keep Wasp's own login forms working, where Clerk cannot.
Clerk is the more instructive example precisely because it is the more constrained one.

## What is identical to the other apps

`schema.prisma`, `src/operations.ts` and `src/MainPage.tsx` are byte-for-byte the same as in
`../wasp-auth` and `../better-auth`. So are `authRequired`, `auth: true`, `useAuth()` and
`logout()`.

## What is specific to this app

- `provider: clerk()` in `main.wasp.ts` — the adapter lives in
  `@wasp.sh/auth-clerk` (`../packages/auth-clerk`). It implements `AuthProvider & SupportsSessionRevocation` and nothing more:
  revocation yes, issuing no.
- `src/auth/LoginPage.tsx` — Clerk's own `<SignIn />`, re-exported by the package. There is no
  `<LoginForm />` here and there cannot be one.
- No client wiring at all: the package ships a client adapter, so Wasp mounts Clerk's context
  and, on the first API call after a Clerk login, exchanges Clerk's token for a Wasp session
  automatically (`POST /auth/login`). There is no `App.tsx` in this app.
- **No user-authored tables. No new routes. No adapter code in `src/`.** Compare
  `../better-auth`, which adds four models, and `../custom-clerk`, which hand-writes this same
  adapter in-app.

## Sessions: exchanged once, then Wasp's own

Clerk is consulted at login (its token is verified once and exchanged for a Wasp session row)
and at logout (dual sign-out: Wasp revokes its own session **and** calls
`clerk.sessions.revokeSession` with the Clerk session the login came from). In between, every
request authenticates against Wasp's session — Clerk is off the request hot path, and a Clerk
outage cannot take down request auth.

The documented gap of minting your own session: revoking the session **in the Clerk dashboard**
does not end the already-exchanged Wasp session; it lives until it expires or the user logs out
of the app.

## What this example is really demonstrating

**Clerk has no server-side password login at all.** Password verification lives on Clerk's
Frontend API behind a browser-held `__client` cookie; the Backend API has no endpoint that turns
credentials into a session. There is no way for Wasp to post credentials on the app's behalf.

That is why the interface splits into two:

```ts
interface AuthProvider {
  authenticate;
} // required of everyone

// capabilities are mixins; an adapter declares what it is by intersection:
interface SupportsSessionRevocation {
  revokeSession;
}
interface SupportsSessionIssuance {
  issueSession;
}

// Clerk:      AuthProvider & SupportsSessionRevocation
// Wasp auth:  AuthProvider & SupportsSessionRevocation
//               & SupportsAllSessionsRevocation & SupportsSessionIssuance
```

Clerk carries only the revocation capability. A uniform `login(email, password)` could be implemented for
Clerk only as something that throws at runtime or silently ignores its arguments — both are lies
a developer discovers in production. A missing capability is the honest alternative.

It also demonstrates the inverse of the usual intuition: **the hosted provider is by far the
easier one to integrate.** Zero tables, zero routes, one small adapter.

## Verified end to end, against a real Clerk instance

The token-verification and provisioning flow below was verified against a real Clerk instance
(before the session exchange landed, Clerk's token was carried on every request; today the same
verification happens once, inside `POST /auth/login`). The exchange and dual sign-out machinery
itself is verified live in `../better-auth`, which runs the identical server code paths with no
external service.

```
GET  /auth/me    (no token)                     200  {"json":null}
GET  /auth/me    (bogus bearer)                 401
POST /auth/login (real Clerk session token)     200  {"sessionId":"…"}   ← the exchange
GET  /auth/me    (Wasp session id)              200  {"id":"0f134392-…","identities":{}}
POST /operations/create-task                    200  task.userId = 0f134392-…
POST /operations/get-my-tasks   (no token)      401
POST /auth/logout                               200  revokes the Wasp session AND the Clerk one
```

Database afterwards — one local `User`, linked to the Clerk subject:

```
local Users: 1
[{ providerName: "clerk",
   providerUserId: "user_3HsJoebz26WA7ijAA3l16xxeC5T",
   authId: "a751e437-…" }]
tasks: [{ description: "created via Clerk auth", userId: "0f134392-…" }]
```

**`/auth/me` returns `0f134392-…`, a uuid from this app's own `User` table — not Clerk's
`user_3HsJoebz…`.** No application code makes that happen: Wasp provisioned the local row the
first time it saw that Clerk subject, inside `resolveExternalSubject`. Exactly the same code
path the Better Auth example uses.

Note also that a bogus token returns **401**, not 500. With a placeholder publishable key it
returns 500 instead — Clerk's SDK rejects the key before it ever looks at the token. That is the
"provider misconfigured" path, and the interface deliberately surfaces it as a 500 so it is not
mistaken for "this user is not signed in".

## Run it

```sh
# after filling in .env.server and .env.client as above
wasp db migrate-dev
wasp start
```
