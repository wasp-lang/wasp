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

All four start as clones of `wasp-auth/` running Wasp's own auth. Each app diverges only
when its provider arrives later in this PR stack, so every diff in these apps from here
on is auth-relevant.

## Running them

Each app is a normal Wasp app:

```sh
cd wasp-auth && wasp db migrate-dev && wasp start
```

See each app's own README for provider-specific setup.
