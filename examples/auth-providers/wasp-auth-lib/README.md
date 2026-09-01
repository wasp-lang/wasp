# Full Wasp auth as a package (POC)

All of Wasp's own auth, externalized into `@wasp.sh/auth`
(see `../packages/auth`) and plugged back in through the provider contract:

- username & password
- email, with verification links and password reset (through the app's
  `emailSender` via the `email-send` grant)
- Google OAuth (PKCE, state cookies, one-time-code handback, redeemed via the
  client adapter's `setSession` sink)

Each method records identities under its own namespace via the
`identity-namespaces` grant; all logins mint Wasp sessions through the
`wasp-sessions` grant.

Run it:

```
cd ../packages/auth && npm install && npm run build
cd ../../wasp-auth-lib && npm install
wasp db migrate-dev
wasp start
```

Dummy emails (verification and reset links) print to the server log.
Google needs real `WASP_AUTH_GOOGLE_*` credentials in `.env.server`; with the
committed dummies the redirect works but the callback exchange fails.
