# Auth schemes — Custom handler (Clerk)

Wasp authenticates every request through **Clerk**, but unlike `../clerk` this app uses no
handler package: both halves are hand-written in `src/auth/` and registered with
`customAuthHandler()`.

```ts
import { createClerkClientAuthHandler } from "./src/auth/clientAuthHandler" with { type: "ref" };
import { createClerkServerAuthHandler } from "./src/auth/handler" with { type: "ref" };

auth: {
  userEntity: "User",
  onAuthFailedRedirectTo: "/login",
  schemes: {
    clerk: customAuthHandler({
      server: {
        authAdapter: createClerkServerAuthHandler,
        env: [/* CLERK_SECRET_KEY, ... */],
      },
      client: {
        authAdapter: createClerkClientAuthHandler,
        env: [/* REACT_APP_CLERK_PUBLISHABLE_KEY */],
      },
    }),
  },
}
```

Both are the same adapters a handler package exports (`createServerAuthHandler`,
`createClientAuthHandler`), so a hand-written scheme has the same powers. Each side holds what that half receives: the
runtime and its declared env vars arrive as arguments, and the client half gets a `Wrapper`, a credential
source, and logout cleanup. The app needs no root component, no env schema and no logout glue
of its own, and `src/MainPage.tsx` is byte-for-byte the shared one.

Clerk's own token is the credential, so the scheme declares no `credentials` and Wasp issues
nothing. Diff this app against `../clerk`: what a handler package absorbs is now just where
the two files live.

## Run it

Same setup as `../clerk` (a free Clerk instance and the same env values); see its README.
