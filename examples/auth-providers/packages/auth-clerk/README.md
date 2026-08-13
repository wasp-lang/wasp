# @wasp.sh/auth-clerk

Clerk as a Wasp auth provider. Verifies every request through Clerk; Wasp
provisions and resolves local users itself.

## Install

```sh
npm install @wasp.sh/auth-clerk @clerk/clerk-react
```

Then:

1. Declare the provider in `main.wasp.ts`:

   ```ts
   import { clerk } from "@wasp.sh/auth-clerk/spec";

   auth: {
     userEntity: "User",
     onAuthFailedRedirectTo: "/login",
     provider: clerk(),
   }
   ```

2. Set the env vars:

   | Var                               | Where  | Notes                                  |
   | --------------------------------- | ------ | -------------------------------------- |
   | `CLERK_SECRET_KEY`                | server | Clerk dashboard → API keys             |
   | `CLERK_PUBLISHABLE_KEY`           | server | Clerk dashboard → API keys             |
   | `CLERK_JWT_KEY`                   | server | optional, networkless JWT verification |
   | `REACT_APP_CLERK_PUBLISHABLE_KEY` | client | same publishable key                   |

3. Wire the client in your root component -- this is all of it:

   ```tsx
   import {
     ClerkAuthProvider,
     useClerkWaspSessionBridge,
   } from "@wasp.sh/auth-clerk/client";
   import { clearSessionId, setSessionId } from "wasp/client/api";

   function Bridge({ children }: { children: React.ReactNode }) {
     useClerkWaspSessionBridge(setSessionId, clearSessionId);
     return <>{children}</>;
   }

   export function App({ children }: { children: React.ReactNode }) {
     return (
       <ClerkAuthProvider>
         <Bridge>{children}</Bridge>
       </ClerkAuthProvider>
     );
   }
   ```

`@wasp.sh/auth-clerk/client` re-exports all of `@clerk/clerk-react`, so login
pages can import `SignIn` and friends from here.

No Prisma models, no routes. Clerk has no server-side password login, so use
Clerk's own `<SignIn />` component -- Wasp's `<LoginForm />` cannot work here.
