---
title: Databases
---

import { Required } from '@site/src/components/Tag'

[Entities](../data-model/entities.md), [Operations](../data-model/operations/overview) and [Automatic CRUD](../data-model/crud.md) together make a high-level interface for working with your app's data. Still, all that data has to live somewhere, so let's see how Wasp deals with databases.

## Supported Database Backends

Wasp supports multiple database backends. We'll list and explain each one.

### SQLite

The default database Wasp uses is [SQLite](https://www.sqlite.org/index.html).

SQLite is a great way for getting started with a new project because it doesn't require any configuration, but Wasp can only use it in development. Once you want to deploy your Wasp app to production, you'll need to switch to PostgreSQL and stick with it.

Fortunately, migrating from SQLite to PostgreSQL is pretty simple, and we have [a guide](#migrating-from-sqlite-to-postgresql) to help you.

### PostgreSQL

[PostgreSQL](https://www.postgresql.org/) is the most advanced open-source database and one of the most popular databases overall.
It's been in active development for 20+ years.
Therefore, if you're looking for a battle-tested database, look no further.

To use Wasp with PostgreSQL, you'll have to ensure a database instance is running during development. Wasp needs access to your database for commands such as `wasp start` or `wasp db migrate-dev` and expects to find a connection string in the `DATABASE_URL` environment variable.

We cover all supported ways of connecting to a database in [the next section](#connecting-to-a-database).

### Migrating from SQLite to PostgreSQL

To run your Wasp app in production, you'll need to switch from SQLite to PostgreSQL.

1. Set the `app.db.system` field to PostgreSQL.
   
```wasp title=main.wasp
app MyApp {
  title: "My app",
  // ...
  db: {
    system: PostgreSQL,
    // ...
  }
}
```

2. Delete all the old migrations, since they are SQLite migrations and can't be used with PostgreSQL, as well as the SQLite database by running [`wasp clean`](https://wasp-lang.dev/docs/general/cli#project-commands):

```bash
rm -r migrations/
wasp clean
```

3. Ensure your new database is running (check the [section on connecting to a database](#connecting-to-a-database) to see how). Leave it running, since we need it for the next step.
4. In a different terminal, run `wasp db migrate-dev` to apply the changes and create a new initial migration.
5. That is it, you are all done!

## Connecting to a Database

Assuming you're not using SQLite, Wasp offers two ways of connecting your app to a database instance:

1. A ready-made dev database that requires minimal setup and is great for quick prototyping.
2. A "real" database Wasp can connect to and use in production.

### Using the Dev Database Provided by Wasp

The command `wasp start db` will start a default PostgreSQL dev database for you.

Your Wasp app will automatically connect to it, just keep `wasp start db` running in the background.
Also, make sure that:

- You have [Docker installed](https://www.docker.com/get-started/) and in `PATH`.
- The port `5432` isn't taken.

### Connecting to an existing database

If you want to spin up your own dev database (or connect to an external one), you can tell Wasp about it using the `DATABASE_URL` environment variable. Wasp will use the value of `DATABASE_URL` as a connection string.

The easiest way to set the necessary `DATABASE_URL` environment variable is by adding it to the [.env.server](../project/env-vars) file in the root dir of your Wasp project (if that file doesn't yet exist, create it).

Alternatively, you can set it inline when running `wasp` (this applies to all environment variables):

```bash
DATABASE_URL=<my-db-url> wasp ...
```

This trick is useful for running a certain `wasp` command on a specific database.
For example, you could do:

```bash
DATABASE_URL=<production-db-url> wasp db seed myProductionSeed
```

This command seeds the data for a fresh staging or production database.
To more precisely understand how seeding works, keep reading.

## Seeding the Database

**Database seeding** is a term used for populating the database with some initial data.

Seeding is most commonly used for two following scenarios:

1.  To put the development database into a state convenient for working and testing.
2.  To initialize any database (`dev`, `staging`, or `prod`) with essential data it requires to operate.
    For example, populating the Currency table with default currencies, or the Country table with all available countries.

### Writing a Seed Function

You can define as many **seed functions** as you want in an array under the `app.db.seeds` field:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=main.wasp
app MyApp {
  // ...
  db: {
    // ...
    seeds: [
      import { devSeedSimple } from "@src/dbSeeds.js",
      import { prodSeed } from "@src/dbSeeds.js"
    ]
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title=main.wasp
app MyApp {
  // ...
  db: {
    // ...
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
This is the same Prisma Client instance that Wasp uses internally and thus includes all of the usual features (e.g., password hashing).

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
              providerData: sanitizeAndSerializeProviderData({
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
import { sanitizeAndSerializeProviderData } from 'wasp/server/auth'
import { type AuthUser } from 'wasp/auth'
import { PrismaClient } from '@prisma/client'

export const devSeedSimple = async (prisma: PrismaClient) => {
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
              providerData: sanitizeAndSerializeProviderData<'username'>({
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

## Prisma Configuration

Wasp uses [Prisma](https://www.prisma.io/) to interact with the database. Prisma is a "Next-generation Node.js and TypeScript ORM" that provides a type-safe API for working with your database.

### Prisma Preview Features

Prisma is still in active development and some of its features are not yet stable. To use them, you have to enable them in the `app.db.prisma.clientPreviewFeatures` field: 

```wasp title="main.wasp"
app MyApp {
  // ...
  db: {
    system: PostgreSQL,
    prisma: {
      clientPreviewFeatures: ["postgresqlExtensions"]
    }
  }
}
```

<small>

Read more about Prisma preview features in the [Prisma docs](https://www.prisma.io/docs/concepts/components/preview-features/client-preview-features).
</small>

### PostgreSQL Extensions

PostgreSQL supports [extensions](https://www.postgresql.org/docs/current/contrib.html) that add additional functionality to the database. For example, the [hstore](https://www.postgresql.org/docs/13/hstore.html) extension adds support for storing key-value pairs in a single column.

To use PostgreSQL extensions with Prisma, you have to enable them in the `app.db.prisma.dbExtensions` field:

```wasp title="main.wasp"
app MyApp {
  // ...
  db: {
    system: PostgreSQL,
    prisma: {
      clientPreviewFeatures: ["postgresqlExtensions"]
      dbExtensions: [
        { name: "hstore", schema: "myHstoreSchema" },
        { name: "pg_trgm" },
        { name: "postgis", version: "2.1" },
      ]
    }
  }
}
```

<small>

Read more about PostgreSQL configuration in Wasp in the [API Reference](#the-appdb-field).
</small>

## API Reference

You can tell Wasp which database to use in the `app` declaration's `db` field:

### The `app.db` Field

Here's an example that uses the `app.db` field to its full potential:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=main.wasp
app MyApp {
  title: "My app",
  // ...
  db: {
    system: PostgreSQL,
    seeds: [
      import devSeed from "@src/dbSeeds.js"
    ],
    prisma: {
      clientPreviewFeatures: ["extendedWhereUnique"]
    }
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title=main.wasp
app MyApp {
  title: "My app",
  // ...
  db: {
    system: PostgreSQL,
    seeds: [
      import devSeed from "@src/dbSeeds.js"
    ],
    prisma: {
      clientPreviewFeatures: ["extendedWhereUnique"]
    }
  }
}
```

</TabItem>
</Tabs>

`app.db` is a dictionary with the following fields (all fields are optional):

- `system: DbSystem`

  The database system Wasp should use. It can be either PostgreSQL or SQLite.
  The default value for the field is SQLite (this default value also applies if the entire `db` field is left unset).
  Whenever you modify the `db.system` field, make sure to run `wasp db migrate-dev` to apply the changes.

- `seeds: [ExtImport]`

  Defines the seed functions you can use with the `wasp db seed` command to seed your database with initial data.
  Read the [Seeding section](#seeding-the-database) for more details.

- `prisma: PrismaOptions`

  Additional configuration for Prisma. 
  
  ```wasp title="main.wasp"
  app MyApp {
    // ...
    db: {
      // ...
      prisma: {
        clientPreviewFeatures: ["postgresqlExtensions"],
        dbExtensions: [
          { name: "hstore", schema: "myHstoreSchema" },
          { name: "pg_trgm" },
          { name: "postgis", version: "2.1" },
        ]
      }
    }
  }
  ```
  
  It's a dictionary with the following fields:

  - `clientPreviewFeatures : [string]`

    Allows you to define [Prisma client preview features](https://www.prisma.io/docs/concepts/components/preview-features/client-preview-features), like for example, `"postgresqlExtensions"`.

  - `dbExtensions: DbExtension[]`

    It allows you to define PostgreSQL extensions that should be enabled for your database. Read more about [PostgreSQL extensions in Prisma](https://www.prisma.io/docs/concepts/components/prisma-schema/postgresql-extensions).

    For each extension you define a dict with the following fields:

    - `name: string` <Required />

      The name of the extension you would normally put in the Prisma file.

      ```prisma title="schema.prisma"
      extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
      //              👆 Extension name
      ```

    - `map: string`

      It sets the `map` argument of the extension. Explanation for the field from the Prisma docs:
      > This is the database name of the extension. If this argument is not specified, the name of the extension in the Prisma schema must match the database name.

    - `schema: string`

      It sets the `schema` argument of the extension. Explanation for the field from the Prisma docs:
      > This is the name of the schema in which to activate the extension's objects. If this argument is not specified, the current default object creation schema is used.

    - `version: string`

      It sets the `version` argument of the extension. Explanation for the field from the Prisma docs:
      > This is the version of the extension to activate. If this argument is not specified, the value given in the extension's control file is used.

### CLI Commands for Seeding the Database

Use one of the following commands to run the seed functions:

- `wasp db seed`

  If you've only defined a single seed function, this command runs it. If you've defined multiple seed functions, it asks you to choose one interactively.

- `wasp db seed <seed-name>`

  This command runs the seed function with the specified name. The name is the identifier used in its `import` expression in the `app.db.seeds` list.
  For example, to run the seed function `devSeedSimple` which was defined like this:

  <Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">

  ```wasp title=main.wasp
  app MyApp {
    // ...
    db: {
      // ...
      seeds: [
        // ...
        import { devSeedSimple } from "@src/dbSeeds.js",
      ]
    }
  }
  ```

  </TabItem>
  <TabItem value="ts" label="TypeScript">

  ```wasp title=main.wasp
  app MyApp {
    // ...
    db: {
      // ...
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
