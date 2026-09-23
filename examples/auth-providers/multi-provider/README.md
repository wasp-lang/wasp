# Auth schemes: waspAuth + Clerk together

Two independent identity systems in one app: Wasp's own username/password auth
next to Clerk, with no account linking. Each scheme carries its own credential
(Wasp's bearer token, Clerk's session token); `user.credentialScheme` says which
one authenticated the request.

```ts
schemes: { wasp: waspAuth({ ... }), clerk: clerk() },
default: "wasp",
```

Pages and operations open to both audiences list both schemes
(`authRequired: ["wasp", "clerk"]`). `/admin` and `getAdminReport` are declared
with `["wasp"]`, so Clerk-authenticated users get an access-denied page and a
403 while wasp-authenticated users get through.

Run it like the sibling examples (Clerk env values as in `../clerk`):

```bash
npm install
wasp db migrate-dev
wasp start
```
