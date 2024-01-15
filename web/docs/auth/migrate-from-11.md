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

:::caution Migrating a deployed app

To migrate a deployed app, you need to go through the migration steps while deploying the app in between the steps.

Read more about migrating a deployed app in the [Migrating a Deployed App](#migrating-a-deployed-app) section.
:::

### 1. Migrate to the New Auth System

If you have an existing app with authentication set up, you can follow these steps to migrate to the new auth system:

1. Upgrade Wasp to the latest version
1. Bump the version to `^0.12.0` in `main.wasp`
1. Create the new auth tables in the database by running:

  ```bash
  wasp db migrate-dev
  ```

1. Create a data migration script in `src/server/migrateToNewAuth.ts`

    - Below you can find [examples of migration scripts](#example-migration-scripts) for each of the auth methods

1. Add the migration script to the `db.seeds` config:
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
1. Run the migration script by running:
  ```bash
  wasp db seed
  ```
1. Verify still works as expected
1. Migrate to using the new auth helper functions for getting the `email` or `username` of the currently logged in user. Read more about the helpers in the [Auth Entities](./entities#accessing-the-auth-fields) section.

### 2. Cleanup the Old Auth System

Now we need to clean up the old auth system:

1. Delete auth-related fields from `User` entity
1. Remove the `externalAuthEntity` from the `auth` config and the `SocialLogin` entity (if you used it)
1. Run `wasp db migrate-dev` again to remove the redundant fields from the database
1. You can now delete the migration script and the `db.seeds` config


## Migrating a Deployed App

To migrate a deployed app, you need to go through the migration steps while deploying the app in between the steps.

1. Go through the steps of [Migrate to the New Auth System](#1-migrate-to-the-new-auth-system) on your local machine _until the step where you run the migration script_
2. Deploy the app (#1)
3. Run the migration script on the production database

    :::info Running the migration script on the production database

    If you have an app that is already deployed, you need to run the migration script on the production database.

    We wrote instructions on how to do it for **Fly.io** deployments here: https://github.com/wasp-lang/wasp/issues/1464 

    The instructions should be similar for other deployment providers: setting up a tunnel from your local machine to the production database and running the migration script locally with `DATABASE_URL` set to the production database.
    :::
4. Finish the steps of [Migrate to the New Auth System](#1-migrate-to-the-new-auth-system) on your local machine
5. Deploy the app (#2)
6. Go through the steps of [Cleanup the Old Auth System](#2-cleanup-the-old-auth-system) on your local machine
7. Deploy the app (#3)

Your app should now be migrated to the new auth system.

## Example Migration Scripts

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
