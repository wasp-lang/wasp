---
title: Databases
---

import { Required } from '@site/src/components/Tag'

[Entities](../data-model/entities.md), [Operations](../data-model/operations/overview) and [Automatic CRUD](../data-model/crud.md) together make a high-level interface for working with your app's data. Still, all that data has to live somewhere, so let's see how Wasp deals with databases.

## Supported Database Backends

Wasp supports multiple database backends. We'll list and explain each one.

### SQLite

The default database Wasp uses is [SQLite](https://www.sqlite.org/index.html).

When you create a new Wasp project, the `schema.prisma` file will have SQLite as the default database provider:

```prisma title="schema.prisma"
datasource db {
  provider = "sqlite"
  url      = env("DATABASE_URL")
}

// ...
```

<small>
  Read more about how Wasp uses the Prisma schema file in the [Prisma schema file](./prisma-file.md) section.
</small>

When you use the SQLite database, Wasp sets the `DATABASE_URL` environment variable for you.

SQLite is a great way to get started with a new project because it doesn't require any configuration, but Wasp can only use it in development. Once you want to deploy your Wasp app to production, you'll need to switch to PostgreSQL and stick with it.

Fortunately, migrating from SQLite to PostgreSQL is pretty simple, and we have [a guide](#migrating-from-sqlite-to-postgresql) to help you.

### PostgreSQL

[PostgreSQL](https://www.postgresql.org/) is the most advanced open-source database and one of the most popular databases overall.
It's been in active development for 20+ years.
Therefore, if you're looking for a battle-tested database, look no further.

To use PostgreSQL with Wasp, set the provider to `"postgresql"` in the `schema.prisma` file:

```prisma title="schema.prisma"
datasource db {
  provider = "postgresql"
  url      = env("DATABASE_URL")
}

// ...
```

<small>
  Read more about how Wasp uses the Prisma schema file in the [Prisma schema file](./prisma-file.md) section.
</small>

You'll have to ensure a database instance is running during development to use PostgreSQL. Wasp needs access to your database for commands such as `wasp start` or `wasp db migrate-dev`.

We cover all supported ways of connecting to a database in [the next section](#connecting-to-a-database).

## Connecting to a Database

### SQLite

If you are using SQLite, you don't need to do anything special to connect to the database. Wasp will take care of it for you.

### PostgreSQL

If you are using PostgreSQL, Wasp supports two ways of connecting to a database:

1. For managed experience, let Wasp spin up a ready-to-go development database for you.
2. For more control, you can specify a database URL and connect to an existing database that you provisioned yourself.

#### Using the Dev Database provided by Wasp

The command `wasp start db` will start a default PostgreSQL dev database for you.

Your Wasp app will automatically connect to it, just keep `wasp start db` running in the background.
Also, make sure that:

- You have [Docker installed](https://www.docker.com/get-started/) and it's available in your `PATH`.
- The port `5432` isn't taken.

:::tip
In case you might want to connect to the dev database through the external tool like `psql` or [pgAdmin](https://www.pgadmin.org/), the credentials are printed in the console when you run `wasp db start`, at the very beginning.
:::

#### Connecting to an existing database

If you want to spin up your own dev database (or connect to an external one), you can tell Wasp about it using the `DATABASE_URL` environment variable. Wasp will use the value of `DATABASE_URL` as a connection string.

The easiest way to set the necessary `DATABASE_URL` environment variable is by adding it to the [.env.server](../project/env-vars) file in the root dir of your Wasp project (if that file doesn't yet exist, create it):

```env title=".env.server"
DATABASE_URL=postgresql://user:password@localhost:5432/mydb
```

Alternatively, you can set it inline when running `wasp` (this applies to all environment variables):

```bash
DATABASE_URL=<my-db-url> wasp ...
```

This trick is useful for running a certain `wasp` command on a specific database.
For example, you could do:

```bash
DATABASE_URL=<production-db-url> wasp db seed myProductionSeed
```

This command seeds the data for a fresh staging or production database. Read more about [seeding the database](#seeding-the-database).

## Migrating from SQLite to PostgreSQL

To run your Wasp app in production, you'll need to switch from SQLite to PostgreSQL.

1. Set the provider to `"postgresql"` in the `schema.prisma` file:

   ```prisma title="schema.prisma"
   datasource db {
     // highlight-next-line
     provider = "postgresql"
     url      = env("DATABASE_URL")
   }

   // ...
   ```

2. Delete all the old migrations, since they are SQLite migrations and can't be used with PostgreSQL, as well as the SQLite database by running [`wasp clean`](../general/cli#project-commands):

   ```bash
   rm -r migrations/
   wasp clean
   ```

3. Ensure your new database is running (check the [section on connecting to a database](#connecting-to-a-database) to see how). Leave it running, since we need it for the next step.

4. In a different terminal, run `wasp db migrate-dev` to apply the changes and create a new initial migration.

5. That is it, you are all done!

## Seeding the Database

**Database seeding** is a term used for populating the database with some initial data.

Seeding is most commonly used for:

1. Getting the development database into a state convenient for working and testing.
2. Initializing any database (`dev`, `staging`, or `prod`) with essential data it requires to operate.
   For example, populating the Currency table with default currencies, or the Country table with all available countries.

### Writing a Seed Function

You can define as many **seed functions** as you want in an array under the `app.db.seeds` field:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```wasp title="main.wasp"
    app MyApp {
      // ...
      db: {
        seeds: [
          import { devSeedSimple } from "@src/dbSeeds.js",
          import { prodSeed } from "@src/dbSeeds.js"
        ]
      }
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```wasp title="main.wasp"
    app MyApp {
      // ...
      db: {
        seeds: [
          import { devSeedSimple } from "@src/dbSeeds.js",
          import { prodSeed } from "@src/dbSeeds.js"
        ]
      }
    }
    ```
  </TabItem>
</Tabs>

Each seed function must be an async function that takes one argument, `prisma`, which is a [Prisma Client](https://www.prisma.io/docs/concepts/components/prisma-client/crud) instance used to interact with the database.
This is the same Prisma Client instance that Wasp uses internally.

Since a seed function falls under server-side code, it can import other server-side functions. This is convenient because you might want to seed the database using Actions.

Here's an example of a seed function that imports an Action:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js
    import { createTask } from './actions.js'
    import { sanitizeAndSerializeProviderData } from 'wasp/server/auth'

    export const devSeedSimple = async (prisma) => {
      const user = await createUser(prisma, {
        username: 'RiuTheDog',
        password: 'bark1234',
      })

      await createTask(
        { description: 'Chase the cat' },
        { user, entities: { Task: prisma.task } }
      )
    }

    async function createUser(prisma, data) {
      const newUser = await prisma.user.create({
        data: {
          auth: {
            create: {
              identities: {
                create: {
                  providerName: 'username',
                  providerUserId: data.username,
                  providerData: await sanitizeAndSerializeProviderData({
                    hashedPassword: data.password
                  }),
                },
              },
            },
          },
        },
      })

      return newUser
    }
    ```

  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts
    import { createTask } from './actions.js'
    import type { DbSeedFn } from 'wasp/server'
    import { sanitizeAndSerializeProviderData } from 'wasp/server/auth'
    import type { AuthUser } from 'wasp/auth'
    import type { PrismaClient } from 'wasp/server'

    export const devSeedSimple: DbSeedFn = async (prisma) => {
      const user = await createUser(prisma, {
        username: 'RiuTheDog',
        password: 'bark1234',
      })

      await createTask(
        { description: 'Chase the cat', isDone: false },
        { user, entities: { Task: prisma.task } }
      )
    };

    async function createUser(
      prisma: PrismaClient,
      data: { username: string, password: string }
    ): Promise<AuthUser> {
      const newUser = await prisma.user.create({
        data: {
          auth: {
            create: {
              identities: {
                create: {
                  providerName: 'username',
                  providerUserId: data.username,
                  providerData: await sanitizeAndSerializeProviderData<'username'>({
                    hashedPassword: data.password
                  }),
                },
              },
            },
          },
        },
      })

      return newUser
    }
    ```

    Wasp exports a type called `DbSeedFn` which you can use to easily type your seeding function.
    Wasp defines `DbSeedFn` like this:

    ```typescript
    type DbSeedFn = (prisma: PrismaClient) => Promise<void>
    ```

    Annotating the function `devSeedSimple` with this type tells TypeScript:

    - The seeding function's argument (`prisma`) is of type `PrismaClient`.
    - The seeding function's return value is `Promise<void>`.

  </TabItem>
</Tabs>

### Running seed functions

Run the command `wasp db seed` and Wasp will ask you which seed function you'd like to run (if you've defined more than one).

Alternatively, run the command `wasp db seed <seed-name>` to choose a specific seed function right away, for example:

```
wasp db seed devSeedSimple
```

Check the [API Reference](#cli-commands-for-seeding-the-database) for more details on these commands.

:::tip
You'll often want to call `wasp db seed` right after you run `wasp db reset`, as it makes sense to fill the database with initial data after clearing it.
:::

## Customising the Prisma Client

Wasp interacts with the database using the [Prisma Client](https://www.prisma.io/docs/orm/prisma-client).
To customize the client, define a function in the `app.db.prismaSetupFn` field that returns a Prisma Client instance.
This allows you to configure features like [logging](https://www.prisma.io/docs/orm/prisma-client/observability-and-logging/logging) or [client extensions](https://www.prisma.io/docs/orm/prisma-client/client-extensions):

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```wasp title=main.wasp
    app MyApp {
      title: "My app",
      // ...
      db: {
        prismaSetupFn: import { setUpPrisma } from "@src/prisma"
      }
    }
    ```

    ```js title="src/prisma.js"
    import { PrismaClient } from '@prisma/client'

    export const setUpPrisma = () => {
      const prisma = new PrismaClient({
        log: ['query'],
      }).$extends({
        query: {
          task: {
            async findMany({ args, query }) {
              args.where = {
                ...args.where,
                description: { not: { contains: 'hidden by setUpPrisma' } },
              }
              return query(args)
            },
          },
        },
      })

      return prisma
    }
    ```

  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```wasp title=main.wasp
    app MyApp {
      title: "My app",
      // ...
      db: {
        prismaSetupFn: import { setUpPrisma } from "@src/prisma"
      }
    }
    ```

    ```ts title="src/prisma.ts"
    import { PrismaClient } from '@prisma/client'

    export const setUpPrisma = () => {
      const prisma = new PrismaClient({
        log: ['query'],
      }).$extends({
        query: {
          task: {
            async findMany({ args, query }) {
              args.where = {
                ...args.where,
                description: { not: { contains: 'hidden by setUpPrisma' } },
              }
              return query(args)
            },
          },
        },
      })

      return prisma
    }
    ```

  </TabItem>
</Tabs>

## API Reference

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```wasp title="main.wasp"
    app MyApp {
      title: "My app",
      // ...
      db: {
        seeds: [
          import devSeed from "@src/dbSeeds"
        ],
        prismaSetupFn: import { setUpPrisma } from "@src/prisma"
      }
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```wasp title="main.wasp"
    app MyApp {
      title: "My app",
      // ...
      db: {
        seeds: [
          import devSeed from "@src/dbSeeds"
        ],
        prismaSetupFn: import { setUpPrisma } from "@src/prisma"
      }
    }
    ```
  </TabItem>
</Tabs>

`app.db` is a dictionary with the following fields (all fields are optional):

- `seeds: [ExtImport]`

  Defines the seed functions you can use with the `wasp db seed` command to seed your database with initial data.
  Read the [Seeding section](#seeding-the-database) for more details.

- `prismaSetupFn: ExtImport`

  Defines a function that sets up the Prisma Client instance. Wasp expects it to return a Prisma Client instance.
  You can use this function to set up [logging](https://www.prisma.io/docs/orm/prisma-client/observability-and-logging/logging) or [client extensions](https://www.prisma.io/docs/orm/prisma-client/client-extensions):

  ```ts title="src/prisma.ts"
  import { PrismaClient } from '@prisma/client'

  export const setUpPrisma = () => {
    const prisma = new PrismaClient({
      log: ['query', 'info', 'warn', 'error'],
    })

    return prisma
  }
  ```

### CLI Commands for Seeding the Database

Use one of the following commands to run the seed functions:

- `wasp db seed`

  If you've only defined a single seed function, this command runs it. If you've defined multiple seed functions, it asks you to choose one interactively.

- `wasp db seed <seed-name>`

  This command runs the seed function with the specified name. The name is the identifier used in its `import` expression in the `app.db.seeds` list.
  For example, to run the seed function `devSeedSimple` which was defined like this:

  <Tabs groupId="js-ts">
    <TabItem value="js" label="JavaScript">
      ```wasp title="main.wasp"
      app MyApp {
        // ...
        db: {
          seeds: [
            // ...
            import { devSeedSimple } from "@src/dbSeeds.js",
          ]
        }
      }
      ```
    </TabItem>

    <TabItem value="ts" label="TypeScript">
      ```wasp title="main.wasp"
      app MyApp {
        // ...
        db: {
          seeds: [
            // ...
            import { devSeedSimple } from "@src/dbSeeds.js",
          ]
        }
      }
      ```
    </TabItem>
  </Tabs>

  Use the following command:

  ```
  wasp db seed devSeedSimple
  ```
