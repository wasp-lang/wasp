# @wasp.sh/auth

Wasp's own auth (username & password, email, OAuth: forms, actions and server flows) as an
auth provider package. The compiler knows nothing about it: it is declared like any other
provider.

```ts
import { waspAuth } from "@wasp.sh/auth/spec";

auth: {
  userEntity: "User",
  onAuthFailedRedirectTo: "/login",
  providers: [waspAuth({ methods: { usernameAndPassword: {} } })],
}
```

- Routes mount at `/auth/wasp/...`. Identities are recorded under `wasp:username`,
  `wasp:email`, `wasp:google`, ...
- Forms and actions: `@wasp.sh/auth/client`. Identity helpers (`getEmail`, `getUsername`):
  `@wasp.sh/auth/user`. Server helpers, hook types and validators: `@wasp.sh/auth/server`.
- OAuth methods need a client route at `/oauth/callback` rendering `OAuthCallbackPage` from
  `@wasp.sh/auth/client`, and declare `JWT_SECRET` plus the provider's client credentials.
