# Auth schemes — Wasp's own auth

Wasp's own auth, declared the way every other scheme is: as a handler package.

```ts
import { waspAuth } from "@wasp.sh/auth/spec";

auth: {
  userEntity: "User",
  onAuthFailedRedirectTo: "/login",
  schemes: { wasp: waspAuth({ methods: { usernameAndPassword: {} } }) },
}
```

The package lives in `../packages/auth`. The compiler knows nothing about it beyond its
manifest: the flows mount at `/auth/wasp/...`, identities live in `wasp:username`, forms come
from `@wasp.sh/auth/client`. By default the scheme runs a private bearer issuer backed by the
`Session` table (`credentials: { transport: "bearer", store: "prisma" }`); `"cookie"` and
`"signed-token"` are one-line switches.

## Run it

```sh
wasp db migrate-dev
wasp start
```

## Tests

```sh
npm run test
```

API specs cover signup, duplicate signup, login, attribution to the `wasp` scheme, a wrong
password, the unprefixed `/auth/username/login` being gone, and logout revocation. One browser
spec signs up, logs out and logs back in through the package's forms.

```
POST /auth/wasp/username/signup                 200  {"success":true}
POST /auth/wasp/username/login                  200  {"credential":"…"}
GET  /auth/me                                   200  sessionScheme = wasp, signedInBy = wasp
POST /auth/username/login                       404
POST /auth/logout                               200
GET  /auth/me      (old credential)             401
```
