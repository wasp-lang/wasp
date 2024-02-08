---
title: Migration from 0.11.X to 0.12.X
---

Wasp made a big change in the way authentication works in version 0.12.0. This guide will help you migrate your app from 0.11.X to 0.12.X.

## What Changed?

### 0.11.X Auth

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

### New Auth

#### Auth Models

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

:::caution Regression Note: Multiple Auth Identities per User

With our old auth implementation, if you were using both Google and email auth methods, your users could sign up with Google first and then, later on, reset their password and therefore also enable logging in with their email and password. This was the only way in which a single user could have multiple login methods at the same time (Google and email).

This is not possible anymore. **The new auth system doesn't support multiple login methods per user at the moment**. We do plan to add this soon though, with the introduction of the [account merging feature](https://github.com/wasp-lang/wasp/issues/954).

If you have any users that have both Google and email login credentials at the same time, you will have to pick only one of those for that user to keep when migrating them.

:::

You can read more about the new auth system in the [Auth Entities](./entities) section.

## How to Migrate?

Migrating your existing app to the new auth system is a two-step process:
1. Migrate to the new auth system
1. Cleanup the old auth system

:::info Migrating a deployed app

While going through these steps, we will focus first on doing the changes locally and your local development database. 

Once we confirm that everything works well, we will also apply those same changes to the deployed app.

**We'll put extra info for migrating a deployed app in a box like this one.**
:::

### 1. Migrate to the New Auth System

You can follow these steps to migrate to the new auth system:

1. Upgrade Wasp to the latest 0.12 version.

  These instructions are for migrating the auth from Wasp `0.11.X` to Wasp `0.12.X`, which means that they work for example for both `0.12.0` and `0.12.5` versions. We suggest that you install the latest 0.12 version of Wasp. Find the available Wasp versions in the [Releases](https://github.com/wasp-lang/wasp/releases) section of our GitHub repo.

  Then you can install that version with:

  ```bash
  curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s -- -v 0.12.0
  ```

  <small>

  In the above command, replace `0.12.0` with the version you want to install.

  </small>

1. Bump the version to `^0.12.0` in `main.wasp`.
1. Create the new auth tables in the database by running:

  ```bash
  wasp db migrate-dev
  ```

  You should see the new `Auth`, `AuthIdentity` and `Session` tables in your database. You can use the `wasp db studio` command to open the database in a GUI and verify that the tables are there.

1. Write your data migration function(s) in `src/migrateToNewAuth.ts`
    - In the previous step, we migrated the schema, and now we need to prepare logic for migrating the data.
    - Below you can find [examples of migration functions](#example-migration-scripts) for each of the auth methods. They should be fine to use as-is, meaning you can just copy them, but you can also modify them to your needs. You will want to have one function per each auth method that you use in your app.
1. Add the migration function(s) to the `db.seeds` config:
  ```wasp title="main.wasp"
  app myApp {
    wasp: {
      version: "^0.12.0"
    },
    // ...
    db: {
      seeds: [
        import { migrateEmailAuth } from "@src/migrateToNewAuth.js",
        import { migrateGoogleAuth } from "@src/migrateToNewAuth.js",
      ]
    },
  }

1. Run the migration function(s) by running:
  ```bash
  wasp db seed
  ```
  If you added multiple migration functions, you can pick which one to run by selecting it from the list.

1. Verify that the auth still works by logging in with each of the auth methods.
1. Update your JS code to work correctly with the new auth entities.

  You should use the new auth helper functions to get the `email` or `username` from a user object. Read more about the helpers in the [Auth Entities](./entities#accessing-the-auth-fields) section.   The helpers you are most likely to use are the `getEmail` and `getUsername` helpers.
1. Finally, check that your app now works as it worked before. If the above steps were done correctly, everything should be working now.

    :::info Migrating a deployed app
    
    After successfully performing migration locally so far, and verifying the your app works as expected, it is time to also migrate our deployed app.
    
    Before migrating your production (deployed) app, we advise you to back up your production database in case something goes wrong. Also, besides testing it in development, it's good to test the migration in a staging environment.
    
    We will perform the production migration in 2 steps:
    - Deploying the new code to production (client and server).
    - Migrating the production database.

    ---
    
    Between these two steps, so after deploying the new code to production and before migrating the production database, your app will not be working completely: new users will be able to sign up, but existing users won't be able to log in, and already logged in users will be logged out. Once you do the second step, migrating the production database, it will all be back to normal.

    You will likely want to keep the time between the two steps as short as you can. Make sure you know exactly what each step means before doing them for real to eliminate any surprises.

    ---
    
    - **First step:** deploy the new code (client and server), either via `wasp deploy` or manually.
    
      Check our [Deployment docs](../advanced/deployment/overview.md) for more details. 

    - **Second step:** run the migration script on the production database with `wasp db seed` command.
    
      We wrote instructions on how to do it for **Fly.io** deployments here: https://github.com/wasp-lang/wasp/issues/1464 . The instructions should be similar for other deployment providers: setting up some sort of an SSH tunnel from your local machine to the production database and running the migration script locally with `DATABASE_URL` pointing to the production database.
    
    Your deployed app should be working normally now, with the new auth system.
    :::


### 2. Cleanup the Old Auth System

Your app should be working correctly and using new auth, but to finish the migration, we need to clean up the old auth system:

1. Delete auth-related fields from `User` entity.

    - This means any fields that were used for authentication, like `email`, `password`, `isEmailVerified`, `emailVerificationSentAt`, `passwordResetSentAt`, `username`, etc.

1. Remove the `externalAuthEntity` from the `auth` config and the `SocialLogin` entity if you used Google or GitHub auth.
1. Run `wasp db migrate-dev` again to remove the redundant fields from the database.
1. You can now delete the migration script and the `db.seeds` config.

:::info Migrating a deployed app

  After doing the steps above successfully locally and making sure everything is working, it is time to push these changes to the deployed app again.
  
  _Deploy the app again_, either via `wasp deploy` or manually. Check our [Deployment docs](../advanced/deployment/overview.md) for more details. 
  
  The database migrations will automatically run on successful deployment of the server and delete the now redundant auth-related `User` columns from the database. 
  
  Your app is now fully migrated to the new auth system.

:::


## Example Migration Functions

The migration functions provided below are written with the typical use cases in mind and you can use them as-is. If your setup requires additional logic, you can use them as a good starting point and modify them to your needs.

### Username & Password

```ts title="src/migrateToNewAuth.ts"
import { PrismaClient } from "@prisma/client";
import { ProviderName, UsernameProviderData } from "wasp/server/auth";

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

```ts title="src/migrateToNewAuth.ts"
import { PrismaClient } from "@prisma/client";
import { EmailProviderData, ProviderName } from "wasp/server/auth";

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

```ts title="src/migrateToNewAuth.ts"
import { PrismaClient } from "@prisma/client";
import { ProviderName } from "wasp/server/auth";

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
