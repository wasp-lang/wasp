# @wasp.sh/auth-better-auth

Better Auth as a Wasp auth provider. Runs in-process, owns its own tables and
HTTP endpoints; Wasp provisions and resolves local users itself.

## Install

```sh
npm install @wasp.sh/auth-better-auth better-auth
```

Then:

1. Declare the provider in `main.wasp.ts`:

   ```ts
   import { betterAuth } from "@wasp.sh/auth-better-auth/spec";

   auth: {
     userEntity: "User",
     onAuthFailedRedirectTo: "/login",
     provider: betterAuth(), // email/password auth, ready to use
     // or: betterAuth({ setupFn }) — your Better Auth config in full (see below)
   }
   ```

   The manifest mounts Better Auth's endpoints at `/better-auth` on the Wasp
   server, with the JSON body parser stripped (Better Auth reads the raw
   stream).

2. Set the env var: `BETTER_AUTH_SECRET` (generate with
   `openssl rand -base64 32`).

3. Add Better Auth's models to `schema.prisma`. The model names are fixed --
   the adapter configures Better Auth with these exact Prisma client
   properties, renamed so they cannot collide with Wasp's own `user`,
   `session` and `account` tables:

   ```prisma
   model BetterAuthUser {
     id            String   @id
     name          String
     email         String   @unique
     emailVerified Boolean  @default(false)
     image         String?
     createdAt     DateTime @default(now())
     updatedAt     DateTime @updatedAt

     sessions BetterAuthSession[]
     accounts BetterAuthAccount[]

     @@map("better_auth_user")
   }

   model BetterAuthSession {
     id        String   @id
     token     String   @unique
     expiresAt DateTime
     ipAddress String?
     userAgent String?
     createdAt DateTime @default(now())
     updatedAt DateTime @updatedAt

     userId String
     user   BetterAuthUser @relation(fields: [userId], references: [id], onDelete: Cascade)

     @@map("better_auth_session")
   }

   model BetterAuthAccount {
     id                    String    @id
     accountId             String
     providerId            String
     accessToken           String?
     refreshToken          String?
     idToken               String?
     accessTokenExpiresAt  DateTime?
     refreshTokenExpiresAt DateTime?
     scope                 String?
     password              String?
     createdAt             DateTime  @default(now())
     updatedAt             DateTime  @updatedAt

     userId String
     user   BetterAuthUser @relation(fields: [userId], references: [id], onDelete: Cascade)

     @@map("better_auth_account")
   }

   model BetterAuthVerification {
     id         String   @id
     identifier String
     value      String
     expiresAt  DateTime
     createdAt  DateTime @default(now())
     updatedAt  DateTime @updatedAt

     @@map("better_auth_verification")
   }
   ```

4. On the client, build Better Auth's client against the Wasp server and use
   its own sign-in methods:

   ```ts
   import { createBetterAuthClient } from "@wasp.sh/auth-better-auth/client";
   import { config } from "wasp/client";

   export const authClient = createBetterAuthClient(config.apiUrl);
   ```

   After a successful sign-in, hand the token to Wasp:
   `setSessionId(result.data.token)` from `wasp/client/api`.
