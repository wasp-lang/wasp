# Auth providers — Wasp's own auth

Wasp's own auth, declared the way every other provider is: as an adapter package.

```ts
import { waspAuth } from "@wasp.sh/auth/spec";

auth: {
  userEntity: "User",
  onAuthFailedRedirectTo: "/login",
  providers: [waspAuth({ methods: { usernameAndPassword: {} } })],
}
```

The package lives in `../packages/auth`. The compiler knows nothing about it beyond its
manifest: the flows mount at `/auth/wasp/...`, identities live in `wasp:username`, forms come
from `@wasp.sh/auth/client`, and the framework keeps only what every provider shares
(sessions, the identity store, `/auth/me`, `/auth/logout`, the credential exchange, hooks).

`src/operations.ts`, `src/MainPage.tsx` and `schema.prisma` are byte-for-byte identical to
the other apps in this directory.

## Run it

```sh
wasp db migrate-dev
wasp start
```

## Tests

```sh
npm run test
```

API specs cover signup, duplicate signup, login, session attribution to `wasp`, a wrong
password, the old unprefixed `/auth/username/login` being gone, and logout revocation. One
browser spec signs up, logs out and logs back in through the package's forms.

## Verified

```
POST /auth/wasp/username/signup                 200  {"success":true}
POST /auth/wasp/username/signup   (again)       422
POST /auth/wasp/username/login                  200  {"sessionId":"…"}
GET  /auth/me                                   200  sessionProviderId = wasp
POST /auth/username/login                       404
POST /auth/logout                               200
GET  /auth/me      (old session)                401
```

```
AuthIdentity.providerName = wasp:username
Session.providerId        = wasp
```
