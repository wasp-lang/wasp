# Auth provider examples

Four Wasp apps that are identical except for **which auth provider verifies the request**.

They exist to answer one question: how much of an app's code survives swapping the auth
provider? The answer, demonstrated rather than asserted, is _everything except the auth pages_.

| App             | Provider                   | What it proves                                                        |
| --------------- | -------------------------- | --------------------------------------------------------------------- |
| `wasp-auth/`    | Wasp's own auth            | The interface is a faithful refactor — behaviour is unchanged         |
| `better-auth/`  | Better Auth, in-process    | A provider that owns its own tables and routes                        |
| `clerk/`        | Clerk, hosted              | A provider with no server-side login at all, and no schema of its own |
| `custom-clerk/` | Clerk, hand-written in-app | The `customAuthProvider()` escape hatch — no adapter package needed   |

## The part that is identical in all of them

```ts
// src/operations.ts — byte-for-byte the same in every app
export const getMyTasks: GetMyTasks<void, Task[]> = async (_args, context) => {
  if (!context.user) throw new HttpError(401);
  return context.entities.Task.findMany({ where: { userId: context.user.id } });
};
```

`context.user` is a row in the app's own `User` table in every app, with the app's own id type.
It is never Clerk's `user_2abc…` string. That is the invariant the provider interface exists to
protect, and it is the one RedwoodJS did not hold: it shipped nine auth adapters over a single
interface but left provisioning to the developer, so `currentUser.id` ended up meaning different
things depending on which adapter was installed.

Also identical: `authRequired` on pages, `auth: true` on operations, `useAuth()`, and `logout()`.

## The part that differs

Only how a session is _established_:

- `wasp-auth` and `better-auth` render login forms and post credentials to the server.
- `clerk` and `custom-clerk` cannot. Clerk has no server-side password endpoint — verification
  lives on its Frontend API behind a browser-held cookie — so those apps use Clerk's own React
  components and Wasp only ever verifies the resulting token.

That asymmetry is why session issuance is a capability (`SupportsSessionIssuance`) layered on
the base `AuthProvider` (verify a request) rather than part of it. Clerk carries only the base
plus revocation.

`clerk/` and `custom-clerk/` are the same provider integrated two ways: through the
`@wasp.sh/auth-clerk` package, and hand-written in the app via `customAuthProvider()`. Diffing
them shows exactly what an adapter package absorbs.

## Running them

Each app is a normal Wasp app:

```sh
cd wasp-auth && wasp db migrate-dev && wasp start
```

`better-auth`, `clerk` and `custom-clerk` need environment variables — see each app's README.
