# @wasp.sh/auth-contract

The contract between Wasp and an auth handler.

A handler package implements it to make any auth solution (Wasp's own auth,
Better Auth, Clerk, ...) a Wasp auth scheme: an `AuthHandler`, built by a
`ServerAuthAdapter` that receives the `WaspServerRuntime`. The client half lives
at `@wasp.sh/auth-contract/client`: `ClientAuthAdapter` and `WaspClientRuntime`.

A package with a spec helper types its adapters from it:

```ts
export const createServerAuthHandler: ServerAuthAdapterFor<typeof myAuth> = (
  runtime,
  spec,
) => { ... };
```

The package is copied into generated Wasp apps as a tarball (like the other libs
in this directory) and installed via a `file:` dependency, so both generated code
and handler packages resolve the same copy.
