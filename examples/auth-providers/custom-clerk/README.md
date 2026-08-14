# Auth providers — Custom adapter (Clerk)

For now a byte-for-byte clone of `../wasp-auth` running Wasp's own auth. It switches to
Clerk later in this PR stack, but unlike `../clerk` it does so **without an adapter
package**: the adapter is hand-written inside the app and declared with
`customAuthProvider()`.

That is the escape hatch for providers nobody has packaged yet, and this app exists to
show what it costs. Compare it with `../clerk`, the same provider behind a package.

The Clerk dependencies are already installed so that the switch shows up as a pure auth
diff, not a lockfile diff.
