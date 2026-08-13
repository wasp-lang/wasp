# Auth provider examples

Three Wasp apps that are identical except for **which auth provider verifies the request**.

They exist to answer one question: how much of an app's code survives swapping the auth
provider? The answer, demonstrated rather than asserted, is *everything except the auth pages*.

| App | Provider | What it proves |
|---|---|---|
| `wasp-auth/` | Wasp's own auth | The interface is a faithful refactor — behaviour is unchanged |
| `better-auth/` | Better Auth, in-process | A provider that owns its own tables and routes |
| `clerk/` | Clerk, hosted | A provider with no server-side login at all, and no schema of its own |

## The part that is identical in all three

```ts
// src/operations.ts — byte-for-byte the same in every app
export const getMyTasks: GetMyTasks<void, Task[]> = async (_args, context) => {
  if (!context.user) throw new HttpError(401)
  return context.entities.Task.findMany({ where: { userId: context.user.id } })
}
```

`context.user` is a row in the app's own `User` table in all three, with the app's own id type.
It is never Clerk's `user_2abc…` string. That is the invariant the provider interface exists to
protect, and it is the one RedwoodJS did not hold: it shipped nine auth adapters over a single
interface but left provisioning to the developer, so `currentUser.id` ended up meaning different
things depending on which adapter was installed.

Also identical: `authRequired` on pages, `auth: true` on operations, `useAuth()`, and `logout()`.

## The part that differs

Only how a session is *established*:

- `wasp-auth` and `better-auth` render login forms and post credentials to the server.
- `clerk` cannot. Clerk has no server-side password endpoint — verification lives on its
  Frontend API behind a browser-held cookie — so the app uses Clerk's own React components and
  Wasp only ever verifies the resulting token.

That asymmetry is why the interface splits `AuthProvider` (verify a request) from
`SessionIssuingAuthProvider` (mint a session server-side). Clerk implements only the first.

## Running them

Each app is a normal Wasp app:

```sh
cd wasp-auth && wasp db migrate-dev && wasp start
```

`better-auth` and `clerk` need environment variables — see each app's own README.
