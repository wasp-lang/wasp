# Auth schemes — Custom password auth

Email+password auth, hand-rolled in-app with `customAuthHandler()` — no handler package, no
auth library. A user-made scheme builds from the same primitives Wasp's own auth uses.

```ts
schemes: {
  password: customAuthHandler({
    server: passwordAuthHandler,
    credentials: {},   // Wasp runs a private bearer issuer for this scheme
  }),
},
```

The primitives, and where this app uses them:

- **The identities facet** (`getSchemeRuntime("password").identities`) — signup creates
  User + Auth + AuthIdentity atomically; the argon2 hash goes into the `secrets` channel, the
  asserted email into `claims`. Hashing happens in this app, explicitly.
- **The credentials facet** — the login `api()` route verifies the password and calls
  `credentials.signIn(subject)`. The app's login hooks fire, the issuer mints a bearer token,
  and the route writes the issuer's answer (`{ credential }`) to the response.
- **The handler** (`src/auth/handler.ts`) forwards `authenticate` to the same issuer, so
  `authRequired: true` recognizes the tokens it handed out.

`schema.prisma`, `src/operations.ts` and `src/MainPage.tsx` are byte-for-byte the same as the
other apps in this directory. Not shown here: email verification, password reset, and
anti-enumeration timing.

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
POST /password-auth/signup                        200  {"success":true}
POST /password-auth/signup   (again)              422
POST /password-auth/login    (wrong password)     401
POST /password-auth/login                         200  {"credential":"…"}
GET  /auth/me                                     200  sessionScheme = password
POST /auth/logout                                 200
GET  /auth/me      (old credential)               401
```
