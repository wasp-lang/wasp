# Auth providers — Clerk

For now a byte-for-byte clone of `../wasp-auth` running Wasp's own auth. It switches to
Clerk via the `@wasp.sh/auth-clerk` adapter package later in this PR stack.

The Clerk client dependencies are already installed so that the switch shows up as a
pure auth diff, not a lockfile diff.
