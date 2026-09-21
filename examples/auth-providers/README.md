# Auth scheme examples

Six Wasp apps that are identical except for **which auth scheme authenticates the request**.

They exist to answer one question: how much of an app's code survives swapping the auth
scheme? The answer, demonstrated rather than asserted, is _everything except the auth pages_.

| App                | Scheme                             | What it proves                                                                                    |
| ------------------ | ---------------------------------- | ------------------------------------------------------------------------------------------------- |
| `wasp-auth/`       | Wasp's own auth (`@wasp.sh/auth`)  | Wasp's own auth is a handler package too: the compiler knows nothing about it beyond its manifest |
| `better-auth/`     | Better Auth, in-process            | A handler that owns its own tables, routes and credential                                         |
| `clerk/`           | Clerk, hosted                      | A handler with no server-side login at all, whose own token is the credential                     |
| `custom-clerk/`    | Clerk, hand-written in-app         | A hand-written handler is the same adapters a package exports, with the same powers               |
| `custom-password/` | Email+password, hand-rolled in-app | A user-made scheme gets Wasp-issued credentials with one line: `credentials: {}`                  |
| `multi-provider/`  | Wasp's own auth + Clerk            | Two schemes side by side, a default, and per-asset scheme lists                                   |

## Vocabulary

- **Handler**: the code. An `AuthHandler` answers `authenticate(request)` and may also
  `signIn`, `signOut`, `challenge` and `forbid`. Packages export handler adapters.
- **Scheme**: a named, configured handler in `auth.schemes`. The name prefixes the handler's
  routes (`/auth/<scheme>/…`) and identity namespaces (`<scheme>:username`), and is what
  `authRequired: ["<scheme>"]` and `user.sessionScheme` refer to.
- **Credentials**: what a request carries. A handler that verifies logins but has no credential
  of its own (Wasp's own auth, the password example) declares `credentials`: inline
  `{ transport: "bearer" | "cookie", store: "prisma" | "signed-token" }` for a private issuer,
  or `{ scheme }` to sign into a sibling `waspBearer()` / `waspCookie()` scheme. Handlers with
  their own credential (Clerk, Better Auth) declare none, and Wasp issues nothing for them.

## The part that is identical in all of them

```ts
// src/operations.ts — byte-for-byte the same in every app
export const getMyTasks: GetMyTasks<void, Task[]> = async (_args, context) => {
  if (!context.user) throw new HttpError(401);
  return context.entities.Task.findMany({ where: { userId: context.user.id } });
};
```

`context.user` is a row in the app's own `User` table in every app, with the app's own id type.
It is never Clerk's `user_2abc…` string. Also identical: `authRequired` on pages, `auth: true`
on operations, `useAuth()`, and `logout()`.

## The part that differs

Only how a login _happens_. `wasp-auth`, `custom-password` and `better-auth` post credentials
to the server. `clerk` and `custom-clerk` cannot: Clerk has no server-side password endpoint,
so those apps use Clerk's own React components and its token rides on every request.

`authRequired: true` means the default scheme (the only scheme, or `auth.default`). A list
(`["wasp", "clerk"]`) is tried in order: the first scheme that authenticates wins, the first
one challenges when none does, and the winner forbids when it is not allowed.

## Running them

Each app is a normal Wasp app:

```sh
cd wasp-auth && wasp db migrate-dev && wasp start
```

`better-auth`, `clerk`, `custom-clerk` and `multi-provider` need environment variables; see each
app's README.
