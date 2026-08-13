# Auth providers — Better Auth

Wasp authenticates every request through **Better Auth**, an in-process auth library that owns
its own tables and its own HTTP endpoints.

```ts
auth: {
  userEntity: "User",
  methods: {},                    // Wasp's own auth is off entirely
  provider: betterAuthProvider,   // Better Auth verifies instead
  onAuthFailedRedirectTo: "/login",
}
```

## What is identical to the other two apps

`schema.prisma` (the `User` and `Task` models), `src/operations.ts` and `src/MainPage.tsx` are
byte-for-byte the same as in `../wasp-auth` and `../clerk`. So are `authRequired`, `auth: true`,
`useAuth()` and `logout()`.

## What is specific to this app

- `src/auth/betterAuth.ts` — a stock Better Auth instance.
- `src/auth/provider.ts` — **the adapter, and it is three methods.**
- `src/auth/routes.ts` + the `api("ALL", ...)` declaration — Better Auth's own endpoints,
  mounted with Wasp's existing user-API mechanism. Note the provider interface has no
  "mount routes" capability and does not need one.
- `src/auth/LoginPage.tsx` — uses Better Auth's client, because Wasp does not wrap login.
- Four `BetterAuth*` models in `schema.prisma`.

## What this example is really demonstrating

**An in-process provider is harder to adopt than a hosted one**, which is the opposite of the
intuition. Compare `../clerk`, which adds _zero_ tables. Better Auth owns its storage, so:

- its four tables live in this app's Prisma schema next to Wasp's own
- every model needs `modelName` set, or `user`/`session`/`account` collide with Wasp's tables
  (and it must be the _Prisma client property_, not the `@@map` name — the adapter does a raw
  `db[modelName]` lookup)
- its routes need the JSON body parser removed, because `toNodeHandler` reads the raw stream.
  Get this wrong and requests hang with no error.

That is worth knowing before betting on Better Auth: the interface is easiest for the provider
Wasp cares least about and hardest for the one actually motivating the work.

## Run it

```sh
printf 'JWT_SECRET=example-app-development-secret-0123456789abcdef\nBETTER_AUTH_SECRET=better-auth-example-secret-0123456789abcdef\n' > .env.server
wasp db migrate-dev
wasp start
```

## Verified

```
POST /better-auth/sign-up/email                 200  {"token":"ISUjd4GE…","user":{…}}
POST /better-auth/sign-in/email                 200  {"token":"BmzL9Xds…"}
GET  /auth/me            (Bearer BA token)      200  {"id":"51666a16-…","identities":{}}
POST /operations/create-task                    200  task.userId = 51666a16-…
POST /operations/get-my-tasks                   200  the task
POST /operations/get-my-tasks   (no token)      401
```

Database afterwards — one Wasp `User`, one Better Auth user, linked by an `AuthIdentity`:

```
wasp Users: 1 | BA users: 1
[{ providerName: "better-auth",
   providerUserId: "Y4jJpb9OoIPjH9n3ZGQ9s61i0FHggjkq",
   authId: "39cdb26c-…" }]
```

**`/auth/me` returns `51666a16-…`, a uuid from this app's own `User` table — not Better Auth's
`Y4jJpb9O…`.** Nobody wrote code to make that happen; Wasp provisioned the local row the first
time it saw that subject. That is the invariant RedwoodJS did not hold.

`identities` is `{}` because no Wasp auth methods are enabled. That is the honest outcome of
`identities` being tiered rather than uniform: its key set depends on the provider.
