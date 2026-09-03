# Auth providers — Custom adapter (Clerk)

Wasp authenticates every request through **Clerk**, but unlike `../clerk` this app uses no
adapter package: the adapter is hand-written in `src/auth/provider.ts` and registered with
`customAuthProvider()`.

```ts
import { clerkAuthProvider } from "./src/auth/provider" with { type: "ref" };

auth: {
  userEntity: "User",
  onAuthFailedRedirectTo: "/login",
  provider: customAuthProvider({
    id: "clerk",
    server: clerkAuthProvider,
    capabilities: ["session-revocation"],
    env: { server: [/* CLERK_SECRET_KEY, ... */], client: [] },
  }),
}
```

That is the escape hatch for providers nobody has packaged yet. This app exists to show what
it costs: ~80 lines of `provider.ts` implementing `authenticate` and `revokeSession`, plus the
client wiring in `src/App.tsx` — a bridge that exchanges Clerk's token for a Wasp session
(`exchangeCredentialForSession`) after a Clerk login, which a package's client adapter would
otherwise do automatically. Diff this app against `../clerk` to see exactly what an adapter
package absorbs.

## Run it

Same setup as `../clerk` (a free Clerk instance and the same four env values); see its README.
