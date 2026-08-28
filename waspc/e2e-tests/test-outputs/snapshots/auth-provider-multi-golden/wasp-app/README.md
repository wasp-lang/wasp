# Auth providers: waspAuth + Clerk together

Two independent identity systems in one app: Wasp's own username/password auth
next to Clerk, with no account linking. Every login ends in the same Wasp
session; `user.sessionProviderId` says which provider minted it.

Shows the provider restriction too: `/admin` and `getAdminReport` are declared
with `["wasp"]`, so Clerk-authenticated users get an access-denied page and a
403 while wasp-authenticated users get through.

Run it like the sibling examples:

```bash
npm install
wasp db migrate-dev
wasp start
```
