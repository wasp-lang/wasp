# @wasp.sh/auth-contract

The contract between Wasp and pluggable auth providers.

An auth adapter package implements this contract to make an external auth
solution (Better Auth, Clerk, WorkOS, ...) usable as a Wasp auth provider:
`AuthProvider`, `VerifiedSession`, `WaspServerRuntime`, and the
`createServerAdapter` factory shape adapter packages must export.

The package is copied into generated Wasp apps as a tarball (like the other libs
in this directory) and installed via a `file:` dependency, so both generated code
and adapter packages resolve the same copy.
