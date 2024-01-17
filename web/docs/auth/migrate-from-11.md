---
title: Migration from 0.11.X
---

Wasp made a big change in the way authentication works in version 0.12.0. This guide will help you migrate your app from 0.11.X to 0.12.X.

## What Changed?

### 0.11.X Auth Models

In 0.11.X, authentication was based on the `User` model which the developer needed to set up properly and take care of the auth fields like `email` or `password`.

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      gitHub: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}

entity User {=psl
  id                        Int           @id @default(autoincrement())
  username                  String        @unique
  password                  String
  externalAuthAssociations  SocialLogin[]
psl=}

entity SocialLogin {=psl
  id          Int       @id @default(autoincrement())
  provider    String
  providerId  String
  user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)
  userId      Int
  createdAt   DateTime  @default(now())
  @@unique([provider, providerId, userId])
psl=}
```

### New Auth Models

From 0.12.X onwards, authentication is based on the auth models which are automatically set up by Wasp. You don't need to take care of the auth fields anymore.

The `User` model is now just a business logic model and you use it for storing the data that is relevant for your app.

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.12.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      gitHub: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}

entity User {=psl
  id Int @id @default(autoincrement())
psl=}
```

You can read more about the new auth system in the [Auth Entities](./entities) section.

## How to Migrate?

Migrating your existing app to the new auth system is a two-step process:
1. Migrate to the new auth system
1. Cleanup the old auth system

:::info Migrating a deployed app

To migrate a deployed app, you need to go through the migration steps while deploying the app in between the steps.

We'll put extra info for migrating a deployed app in a box like this one.
:::

### 1. Migrate to the New Auth System

If you have an existing app with authentication set up, you can follow these steps to migrate to the new auth system:

1. Upgrade Wasp to the latest 0.12 version.
1. Bump the version to `^0.12.0` in `main.wasp`.
1. Create the new auth tables in the database by running:

  ```bash
  wasp db migrate-dev
  ```

  You should see the new `Auth`, `AuthIdentity` and `Session` tables in your database.

1. Create a data migration script in `src/server/migrateToNewAuth.ts`

    - Below you can find [examples of migration scripts](#example-migration-scripts) for each of the auth methods. They should be fine to use as-is, but you can also modify them to your needs.

1. Add the migration script(s) to the `db.seeds` config:
  ```wasp title="main.wasp"
  app myApp {
    wasp: {
      version: "^0.12.0"
    },
    // ...
    db: {
      seeds: [
        import { migrateEmailAuth } from "@server/migrateToNewAuth.js",
        import { migrateGoogleAuth } from "@server/migrateToNewAuth.js",
      ]
    },
  }
  ```
1. Run the migration script(s) by running:
  ```bash
  wasp db seed
  ```
  If you have multiple migration scripts, you can pick which one to run by selecting it from the list.

  :::info Migrating a deployed app

    _First, deploy your app_ with the changes from previous steps. Then you'll need to run the migration script on the production database with `wasp db seed` command.

    **Running the migration script on the production database**

    We wrote instructions on how to do it for **Fly.io** deployments here: https://github.com/wasp-lang/wasp/issues/1464 

    The instructions should be similar for other deployment providers: setting up some sort of an SSH tunnel from your local machine to the production database and running the migration script locally with `DATABASE_URL` pointing to the production database.
  :::

1. Migrate to using the new auth helper functions for getting the `email` or `username` of the currently logged-in user. Read more about the helpers in the [Auth Entities](./entities#accessing-the-auth-fields) section.

  The helpers you'll probably use are the `getEmail` and `getUsername` helpers.

  :::info Migrating a deployed app

    Now, _deploy the app again_ with the new auth helpers in place.

  :::

1. Verify that the auth still works by logging in with each of the auth methods.

### 2. Cleanup the Old Auth System

Now we need to clean up the old auth system:

1. Delete auth-related fields from `User` entity.
1. Remove the `externalAuthEntity` from the `auth` config and the `SocialLogin` entity (if you used it).
1. Run `wasp db migrate-dev` again to remove the redundant fields from the database.
1. You can now delete the migration script and the `db.seeds` config.

:::info Migrating a deployed app

  One last time, _deploy the app_ with the old auth system cleaned up. Your app should now be migrated to the new auth system.

:::


### Expect Some Downtime

Expect some downtime when migrating a deployed app. The downtime should be minimal, but the auth system won't work 100% while the migration is in progress.

As soon as you deploy the new auth system, users will be able to sign up immediately, but existing users won't be able to log in until you run the migration script on the production database.

## Example Migration Scripts

The provided migration scripts should be fine to use as-is, but you can also modify them to your needs.

### Username & Password

```ts title="src/server/migrateToNewAuth.ts"
import { PrismaClient } from "@prisma/client";
import { ProviderName, UsernameProviderData } from "@wasp/auth/utils";

export async function migrateUsernameAuth(prismaClient: PrismaClient) {
  const users = await prismaClient.user.findMany({
    include: {
      auth: true,
    },
  });

  for (const user of users) {
    if (user.auth) {
      console.log("User was already migrated, skipping", user);
      continue;
    }

    if (!user.username || !user.password) {
      console.log("Missing username auth info, skipping user", user);
      continue;
    }

    const providerData: UsernameProviderData = {
      hashedPassword: user.password,
    };
    const providerName: ProviderName = "username";

    await prismaClient.auth.create({
      data: {
        identities: {
          create: {
            providerName,
            providerUserId: user.username.toLowerCase(),
            providerData: JSON.stringify(providerData),
          },
        },
        user: {
          connect: {
            id: user.id,
          },
        },
      },
    });
  }
}
```


### Email

```ts title="src/server/migrateToNewAuth.ts"
import { PrismaClient } from "@prisma/client";
import { EmailProviderData, ProviderName } from "@wasp/auth/utils";

export async function migrateEmailAuth(prismaClient: PrismaClient) {
  const users = await prismaClient.user.findMany({
    include: {
      auth: true,
    },
  });

  for (const user of users) {
    if (user.auth) {
      console.log("User was already migrated, skipping", user);
      continue;
    }

    if (!user.email || !user.password) {
      console.log("Missing email auth info, skipping user", user);
      continue;
    }

    const providerData: EmailProviderData = {
      isEmailVerified: user.isEmailVerified,
      emailVerificationSentAt:
        user.emailVerificationSentAt?.toISOString() ?? null,
      passwordResetSentAt: user.passwordResetSentAt?.toISOString() ?? null,
      hashedPassword: user.password,
    };
    const providerName: ProviderName = "email";

    await prismaClient.auth.create({
      data: {
        identities: {
          create: {
            providerName,
            providerUserId: user.email,
            providerData: JSON.stringify(providerData),
          },
        },
        user: {
          connect: {
            id: user.id,
          },
        },
      },
    });
  }
}
```


### Google & GitHub

```ts title="src/server/migrateToNewAuth.ts"
import { PrismaClient } from "@prisma/client";
import { ProviderName } from "@wasp/auth/utils";

export async function migrateGoogleAuth(prismaClient: PrismaClient) {
  return createSocialLoginMigration(prismaClient, "google");
}

export async function migrateGitHubAuth(prismaClient: PrismaClient) {
  return createSocialLoginMigration(prismaClient, "github");
}

async function createSocialLoginMigration(
  prismaClient: PrismaClient,
  providerName: "google" | "github"
) {
  const users = await prismaClient.user.findMany({
    include: {
      auth: true,
      externalAuthAssociations: true,
    },
  });

  for (const user of users) {
    if (user.auth) {
      console.log("User was already migrated, skipping", user);
      continue;
    }

    const provider = user.externalAuthAssociations.find(
      (provider) => provider.provider === providerName
    );

    if (!provider) {
      console.log(`Missing ${providerName} provider, skipping user`, user);
      continue;
    }

    await prismaClient.auth.create({
      data: {
        identities: {
          create: {
            providerName,
            providerUserId: provider.providerId,
            providerData: JSON.stringify({}),
          },
        },
        user: {
          connect: {
            id: user.id,
          },
        },
      },
    });
  }
}
```
