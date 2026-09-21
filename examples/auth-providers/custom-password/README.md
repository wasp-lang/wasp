# Auth schemes — Custom password auth

Email+password auth, hand-rolled in-app with `customAuthHandler()` — no handler package, no
auth library. A user-made scheme builds from the same primitives Wasp's own auth uses.

```ts
schemes: {
  password: customAuthHandler({
    server: {
      authAdapter: createPasswordAuthHandler, // like a package's createServerAuthHandler
      routes: {},                                     // it brings its own, mounted at /auth/password
    },
    credentials: {}, // Wasp runs a private bearer issuer for this scheme
  }),
},
```

The primitives, and where this app uses them:

`server.authAdapter` is the same thing a handler package exports, so the scheme's runtime arrives as an
argument and the adapter may return routes of its own. Everything lives in
`src/auth/handler.ts`:

- **The identities facet** (`runtime.identities`) — signup creates
  User + Auth + AuthIdentity atomically; the argon2 hash goes into the `secrets` channel, the
  asserted email into `claims`. Hashing happens in this app, explicitly.
- **The credentials issuer** — the login route verifies the password and calls
  `credentials.signIn(subject)`. The app's login hooks fire, the issuer mints a bearer token,
  and the route writes the issuer's answer (`{ credential }`) to the response.
- **The handler** forwards `authenticate` to the same issuer, so
  `authRequired: true` recognizes the tokens it handed out.

`schema.prisma`, `src/operations.ts` and `src/MainPage.tsx` are byte-for-byte the same as the
other apps in this directory. Not shown here: email verification, password reset, and
anti-enumeration timing.

Prefer ordinary Wasp `api()` routes? Keep the adapter for `handler`, stash `runtime` in a
module variable, and read it from those routes. That is plain userland; Wasp needs no API for it.

## Run it

```sh
wasp db migrate-dev
wasp start
```

## Tests

```sh
npm run test
```

```
POST /auth/password/signup                        200  {"success":true}
POST /auth/password/signup   (again)              422
POST /auth/password/login    (wrong password)     401
POST /auth/password/login                         200  {"credential":"…"}
GET  /auth/me                                     200  sessionScheme = password
POST /auth/logout                                 200
GET  /auth/me      (old credential)               401
```
