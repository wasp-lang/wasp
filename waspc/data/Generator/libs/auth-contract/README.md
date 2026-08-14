# @wasp.sh/auth-contract

The contract between Wasp and pluggable auth providers.

An auth provider implements this contract to make an external auth solution
(Better Auth, Clerk, WorkOS, ...) usable as a Wasp auth provider: `AuthProvider`,
`VerifiedSession`, and the optional `SessionIssuingAuthProvider` capability.

The package is copied into generated Wasp apps as a tarball (like the other libs
in this directory) and installed via a `file:` dependency, so both generated code
and adapter packages resolve the same copy.
