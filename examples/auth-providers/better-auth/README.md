# Auth providers — Better Auth

For now a byte-for-byte clone of `../wasp-auth` running Wasp's own auth. It switches to
Better Auth via the `@wasp.sh/auth-better-auth` adapter package later in this PR stack.

Two things are already in place so that the switch shows up as a pure auth diff:

- the `better-auth` dependency is installed, and
- `schema.prisma` carries Better Auth's own tables (`BetterAuth*`). They are plain
  Prisma models and sit unused until the provider arrives.
