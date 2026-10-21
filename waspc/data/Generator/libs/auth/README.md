# Auth Library

Wasp's auth as one package: sessions (Lucia), the `AuthUser` shape, auth identity
lookup and creation, provider data storage, the auth hooks runner, the request-to-user
middleware, the `/auth/me` and `/auth/logout` handlers, and the auth methods Wasp ships
(email, username and password, OAuth providers) with their server flows and client forms.

Everything the app decides (entity names, relation field names, enabled providers, hook
functions, user signup fields, email content, redirect paths, API paths) is passed in by
the generated code through factory functions.

Module layout under `src/`:

- `shared/`: runtime-agnostic code used by both the node and browser entries:
  `providerData.ts`, `authUser.ts`, `entities.ts`, `errors.ts`, `validation.ts`,
  `email.ts`, `emailResend.ts`, `responseSchemas.ts`; `shared/index.ts` is the `.` entry
- `node/`: `sessions.ts`, `identities.ts`, `hooks.ts`, `middleware.ts`, `routes.ts`,
  `password.ts`, `providerDataStorage.ts`, `jwt.ts`
- `node/providers/{email,username,oauth}/`: the server flows of each auth method
- `browser/`: `authQuery.ts`, `logout.ts`, `context.ts`, `actions/`, `forms/`

Entry points:

- `@wasp.sh/lib-auth`
- `@wasp.sh/lib-auth/node`
- `@wasp.sh/lib-auth/browser`

See the [Wasp libs conventions](../README.md) for development and packaging.
