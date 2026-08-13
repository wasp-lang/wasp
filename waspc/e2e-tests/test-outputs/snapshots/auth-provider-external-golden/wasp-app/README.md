# Auth providers — Clerk

Wasp authenticates every request through **Clerk**, a hosted provider.

```ts
auth: {
  userEntity: "User",
  methods: {},                 // Wasp's own auth is off entirely
  provider: clerkAuthProvider, // Clerk verifies instead
  onAuthFailedRedirectTo: "/login",
}
```

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
blocking registration hooks. So a WorkOS adapter could implement
`SessionIssuingAuthProvider` and keep Wasp's own login forms working, where Clerk cannot.
Clerk is the more instructive example precisely because it is the more constrained one.

## What is identical to the other two apps

`schema.prisma`, `src/operations.ts` and `src/MainPage.tsx` are byte-for-byte the same as in
`../wasp-auth` and `../better-auth`. So are `authRequired`, `auth: true`, `useAuth()` and
`logout()`.

## What is specific to this app

- `src/auth/provider.ts` — the adapter. Implements `AuthProvider` and **not**
  `SessionIssuingAuthProvider`.
- `src/auth/LoginPage.tsx` — Clerk's own `<SignIn />`. There is no `<LoginForm />` here and
  there cannot be one.
- `src/App.tsx` — bridges Clerk's token into Wasp's client credential store.
- **No new tables. No new routes.** Compare `../better-auth`, which adds four models and a
  catch-all route.

## What this example is really demonstrating

**Clerk has no server-side password login at all.** Password verification lives on Clerk's
Frontend API behind a browser-held `__client` cookie; the Backend API has no endpoint that turns
credentials into a session. There is no way for Wasp to post credentials on the app's behalf.

That is why the interface splits into two:

```ts
interface AuthProvider {
  verifyRequest;
  verifyCredential;
  revokeSession;
}
interface SessionIssuingAuthProvider extends AuthProvider {
  issueSession;
  revokeAllSessions;
}
```

Clerk implements only the first. A uniform `login(email, password)` could be implemented for
Clerk only as something that throws at runtime or silently ignores its arguments — both are lies
a developer discovers in production. A missing capability is the honest alternative.

It also demonstrates the inverse of the usual intuition: **the hosted provider is by far the
easier one to integrate.** Zero tables, zero routes, one small adapter.

## Verified end to end, against a real Clerk instance

```
GET  /auth/me   (no token)                      200  {"json":null}
GET  /auth/me   (bogus bearer)                  401
GET  /auth/me   (real Clerk session token)      200  {"id":"0f134392-…","identities":{}}
POST /operations/create-task                    200  task.userId = 0f134392-…
POST /operations/get-my-tasks   (no token)      401
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
