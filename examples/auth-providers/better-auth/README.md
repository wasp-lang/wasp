# Auth providers — Better Auth

Wasp authenticates every request through **Better Auth**, an in-process auth library that owns
its own tables and its own HTTP endpoints, via the `@wasp.sh/auth-better-auth` adapter package
(`../packages/auth-better-auth`).

```ts
import { betterAuth } from "@wasp.sh/auth-better-auth/spec";

auth: {
  userEntity: "User",
  onAuthFailedRedirectTo: "/login",
  provider: betterAuth({ setupFn: setupBetterAuth }), // Better Auth verifies instead
}
```

That one call carries the whole server-side integration: the adapter, Better Auth's own routes
(mounted at `/better-auth` with the JSON body parser stripped), and the `BETTER_AUTH_SECRET`
requirement. What the manifest cannot carry is the Prisma schema -- the four `BetterAuth*`
models still live in this app's `schema.prisma`.

## What is identical to the other apps

`schema.prisma` (the `User` and `Task` models), `src/operations.ts` and `src/MainPage.tsx` are
byte-for-byte the same as in `../wasp-auth` and `../clerk`. So are `authRequired`, `auth: true`,
`useAuth()` and `logout()`.

## What is specific to this app

- `provider: betterAuth(...)` in `main.wasp.ts` — the adapter lives in
  `@wasp.sh/auth-better-auth` (`../packages/auth-better-auth`): the Better Auth instance, the
  provider's two methods, and the route handler, all built by one `createServerAdapter` factory.
- The manifest's `routes` declaration — Better Auth's own endpoints, mounted by Wasp at
  `/better-auth`. An earlier version of this app declared them by hand with `api("ALL", ...)`
  plus an `apiNamespace` middleware tweak; the manifest replaces both.
- `src/auth/authClient.ts` — two lines: point the package's client at the Wasp server.
- `src/auth/LoginPage.tsx` — uses Better Auth's client, because Wasp does not wrap login. After
  a successful sign-in it exchanges Better Auth's token for a Wasp session
  (`exchangeCredentialForSession`), which every subsequent request authenticates with.
- Four `BetterAuth*` models in `schema.prisma`.

## Sessions: two rows per login, on purpose

A login here produces a `BetterAuthSession` row (Better Auth's) and a `Session` row (Wasp's,
minted by the exchange, holding a pointer to the Better Auth one). That duplication is the
accepted price of uniform sessions: every provider — hosted, in-process, or a plain token
verifier — gets the same instant Wasp-side revocation and the same logout semantics. Logout is
dual sign-out: Wasp deletes its session row **and** the Better Auth session it was exchanged
from. The gap in the other direction is documented: deleting only the Better Auth row leaves the
Wasp session alive until expiry or logout.

## What this example is really demonstrating

**An in-process provider is harder to adopt than a hosted one**, which is the opposite of the
intuition. Compare `../clerk`, which adds _zero_ tables. Better Auth owns its storage, and the
adapter package can absorb most but not all of that:

- absorbed: every model needs `modelName` set, or `user`/`session`/`account` collide with
  Wasp's tables (and it must be the _Prisma client property_, not the `@@map` name — the
  adapter does a raw `db[modelName]` lookup). The package sets these.
- absorbed: its routes need the JSON body parser removed, because `toNodeHandler` reads the
  raw stream. Get this wrong and requests hang with no error. The manifest's
  `routes: { rawBody: true }` handles it.
- not absorbable: its four tables live in this app's Prisma schema next to Wasp's own, pasted
  from the package's README. A manifest cannot contribute Prisma models.

That is worth knowing before betting on Better Auth: the interface is easiest for the provider
Wasp cares least about and hardest for the one actually motivating the work.

## Run it

```sh
printf 'BETTER_AUTH_SECRET=better-auth-example-secret-0123456789abcdef\n' > .env.server
wasp db migrate-dev
wasp start
```

## Verified

```
POST /better-auth/sign-up/email                 200  {"token":"0vPcyGjQ…","user":{…}}
POST /auth/login         (Bearer BA token)      200  {"sessionId":"dld7oebe…"}   ← the exchange
GET  /auth/me            (Wasp session id)      200  {"id":"603f28ba-…","identities":{}}
POST /operations/create-task                    200  task.userId = 603f28ba-…
GET  /auth/me            (Bearer BA token)      401  ← the provider is off the hot path
POST /auth/logout                               200  {"success":true}
GET  /auth/me            (old Wasp session)     401
POST /auth/login         (old BA token)         401  ← upstream session revoked too
```

Database before logout — the Wasp `Session` row points at the Better Auth session it was
exchanged from; after logout, **both are gone** (dual sign-out):

```
Session:             dld7oebe… | providerSessionId = vLYtrCE1…
better_auth_session: vLYtrCE1…
-- after POST /auth/logout --
wasp sessions: 0 | BA sessions: 0
```

**`/auth/me` returns `603f28ba-…`, a uuid from this app's own `User` table — not Better Auth's
subject id.** Nobody wrote code to make that happen; Wasp provisioned the local row during the
exchange. That is the invariant RedwoodJS did not hold.

`identities` is `{}` because no Wasp auth methods are enabled. That is the honest outcome of
`identities` being tiered rather than uniform: its key set depends on the provider.
