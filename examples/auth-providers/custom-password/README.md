# Auth providers — Custom password auth

Email+password auth, hand-rolled in-app with `customAuthProvider()` — no adapter package, no
auth library, ~150 lines. This app is the proof that "no cheating" holds: a user-made provider
builds from byte-for-byte the same primitives Wasp's own auth uses.

```ts
provider: customAuthProvider({
  id: "external:password",
  server: passwordAuthProvider,
  capabilities: [],   // a stateless verifier has no provider session to issue or revoke
  env: { server: [], client: [] },
}),
```

The three primitives, and where this app uses them:

- **The identity store** (`getIdentityStore("external:password")`) — signup creates
  User + Auth + AuthIdentity atomically; the argon2 hash goes into the `secrets` channel (the
  column the Prisma client omits by default), the asserted email into `claims`. Hashing happens
  in this app, explicitly — Wasp ships no crypto to providers.
- **The session exchange** — login sends `Authorization: Basic …` to Wasp's own
  `POST /auth/login`. The provider's `authenticate` verifies the password once, returns no
  `sessionId` (it has no session of its own), and Wasp mints the first-party session every
  subsequent request authenticates with.
- **An `api()` route** for signup — the provider's only endpoint, declared like any other.

`schema.prisma`, `src/operations.ts` and `src/MainPage.tsx` are byte-for-byte the same as the
other apps in this directory. Deliberately not shown here: email verification, password reset,
and anti-enumeration timing — compare Wasp's own email auth for those.

## Run it

```sh
wasp db migrate-dev
wasp start
```

## Verified

```
POST /password-auth/signup                        200  {"success":true}
POST /password-auth/signup   (again)              422  already exists (P2002 via the store)
POST /auth/login   (Basic, wrong password)        401
POST /auth/login   (Basic, correct)               200  {"sessionId":"bd7y2aiz…"}
GET  /auth/me      (Wasp session)                 200  {"id":"ee54ae39-…","identities":{}}
POST /operations/create-task                      200  task.userId = ee54ae39-…
POST /auth/logout                                 200  (no provider session to revoke — Wasp's is the only one)
GET  /auth/me      (old session)                  401
```

The identity row afterwards — claims, data, and secrets in their own columns, the hash in the
one the Prisma client omits by default:

```
providerName    = external:password
providerUserId  = pw-test-…@example.com
providerClaims  = {"email":"pw-test-…@example.com"}
providerData    = {}
providerSecrets = {"hashedPassword":"$argon2id$v=19$…"}   ← server-only
```
