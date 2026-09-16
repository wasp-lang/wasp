# @wasp.sh/auth

Wasp's own auth (username & password, email, OAuth: forms, actions and server flows) as an
auth handler package. The compiler knows nothing about it: it is declared like any other
scheme.

```ts
import { waspAuth } from "@wasp.sh/auth/spec";

auth: {
  userEntity: "User",
  onAuthFailedRedirectTo: "/login",
  schemes: { wasp: waspAuth({ methods: { usernameAndPassword: {} } }) },
}
```

- Routes mount at `/auth/<scheme>/...`. Identities are recorded under `<scheme>:username`,
  `<scheme>:email`, `<scheme>:google`, ...
- `credentials` picks how a verified login turns into the credential the client carries:
  the default is a private bearer issuer backed by the `Session` table
  (`{ transport: "bearer", store: "prisma" }`); `"cookie"` and `"signed-token"` are
  alternatives, and `{ scheme }` signs into a sibling `waspBearer()` / `waspCookie()`.
- Forms and actions: `@wasp.sh/auth/client`. Identity helpers (`getEmail`, `getUsername`):
  `@wasp.sh/auth/user`. Server helpers, hook types and validators: `@wasp.sh/auth/server`.
- OAuth methods need a client route at `/oauth/callback` rendering `OAuthCallbackPage` from
  `@wasp.sh/auth/client`, and declare `JWT_SECRET` plus the provider's client credentials.
