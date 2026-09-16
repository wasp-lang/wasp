# Auth schemes — Custom handler (Clerk)

Wasp authenticates every request through **Clerk**, but unlike `../clerk` this app uses no
handler package: the handler is hand-written in `src/auth/handler.ts` and registered with
`customAuthHandler()`.

```ts
import { clerkAuthHandler } from "./src/auth/handler" with { type: "ref" };

auth: {
  userEntity: "User",
  onAuthFailedRedirectTo: "/login",
  schemes: {
    clerk: customAuthHandler({
      server: clerkAuthHandler,
      env: { server: [/* CLERK_SECRET_KEY, ... */], client: [] },
    }),
  },
}
```

Clerk's own token is the credential, so the scheme declares no `credentials` and Wasp issues
nothing. What the escape hatch costs: `handler.ts` implementing `authenticate` and `signOut`,
plus `src/App.tsx`, which registers Clerk's token as the credential source Wasp's client puts on
every request — what a package's client adapter would otherwise do. Diff this app against
`../clerk` to see exactly what a handler package absorbs.

## Run it

Same setup as `../clerk` (a free Clerk instance and the same env values); see its README.
