# Auth schemes — Better Auth

Wasp authenticates every request through **Better Auth**, an in-process auth library that owns
its own tables and HTTP endpoints, via the `@wasp.sh/auth-better-auth` handler package
(`../packages/auth-better-auth`).

```ts
import { betterAuth } from "@wasp.sh/auth-better-auth/spec";

auth: {
  userEntity: "User",
  onAuthFailedRedirectTo: "/login",
  schemes: { "better-auth": betterAuth({ setupFn: setupBetterAuth }) },
}
```

That one call carries the server-side integration: the handler, Better Auth's own routes
(mounted at `/auth/better-auth` with the JSON body parser stripped), and the
`BETTER_AUTH_SECRET` requirement. What the manifest cannot carry is the Prisma schema: the four
`BetterAuth*` models live in this app's `schema.prisma`, pasted from the package's README.

## How it works

Better Auth's session token **is** the credential. `src/auth/authClient.ts` creates the
package's client, which hands each fresh token to Wasp after a successful sign-in; every
request then carries it and the handler verifies it with `auth.api.getSession`. The scheme
declares no `credentials`, so Wasp issues nothing. `logout()` signs the Better Auth session
out through its own API.

`src/auth/LoginPage.tsx` uses Better Auth's client methods, because Wasp does not wrap login;
only reading the session is uniform across schemes.

## Run it

```sh
printf 'BETTER_AUTH_SECRET=better-auth-example-secret-0123456789abcdef\n' > .env.server
wasp db migrate-dev
wasp start
```
