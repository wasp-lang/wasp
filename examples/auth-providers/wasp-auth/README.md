# Auth providers — Wasp's own auth

The baseline. `app.auth.provider` is **not set**, which is the default and means "use Wasp's
built-in auth" — exactly what every existing Wasp app does today.

```ts
auth: {
  userEntity: "User",
  methods: { usernameAndPassword: {} },
  onAuthFailedRedirectTo: "/login",
}
```

It exists so the other two apps have something to be compared against: `src/operations.ts`,
`src/MainPage.tsx` and `schema.prisma` here are byte-for-byte identical to the Better Auth and
Clerk versions.

## Run it

```sh
echo 'JWT_SECRET=example-app-development-secret-0123456789abcdef' > .env.server
wasp db migrate-dev
wasp start
```

## Verified

```
POST /auth/username/signup                      200  {"success":true}
POST /auth/username/login                       200  {"sessionId":"wznhanafh4ilzy…"}
GET  /auth/me                                   200  {"id":"b71f739e-…","identities":{"username":{"id":"alice"}}}
POST /operations/create-task    (with token)    200  task created with userId = b71f739e-…
POST /operations/get-my-tasks   (with token)    200  the task
POST /operations/get-my-tasks   (no token)      401
```

Note the `userId` on the created task is the app's own `User.id`. That is the invariant the
provider interface protects, and it holds identically in the other two apps.
