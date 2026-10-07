import { CardLink } from '@site/src/components/CardLink'

# Databases

[Entities](./entities.md), [Operations](./operations/overview) and [Automatic CRUD](./crud.md) together make a high-level interface for working with your app's data. Still, all that data has to live somewhere, so let's see how Wasp deals with databases.

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

Wasp can start a PostgreSQL development database for commands such as `wasp start` or `wasp db migrate-dev`.

We cover all supported ways of connecting to a database in [the next section](#connecting-to-a-database).

## Connecting to a Database

### SQLite

If you are using SQLite, you don't need to do anything special to connect to the database. Wasp will take care of it for you without starting a Docker database.

### PostgreSQL

If you are using PostgreSQL, Wasp supports two ways of connecting to a database:

1. For managed experience, let Wasp spin up a ready-to-go development database for you.
2. For more control, you can specify a database URL and connect to an existing database that you provisioned yourself.

#### Using the Dev Database provided by Wasp

Run migrations, then start the app. Wasp starts the development database automatically:

```bash
wasp db migrate-dev
wasp start
```

Wasp runs the development database in [Docker](https://www.docker.com/get-started/). Each command starts it if needed, waits until it is ready, and shows its logs. When the command finishes, Wasp stops and removes the container. Database files are stored in a Docker volume and reused on the next run.

If the database is already running, Wasp uses it without stopping it afterward. This applies to `wasp start`, `wasp db migrate-dev`, `wasp db reset`, `wasp db seed`, and `wasp db studio`.

Stop `wasp start` before running `wasp db migrate-dev`, `wasp db reset`, `wasp db seed`, or `wasp db studio`. These commands need the same project lock.

To keep the database running across commands, run `wasp db start` in a separate terminal. This command fails if the development database is already running. It prints connection credentials for tools such as `psql` or [pgAdmin](https://www.pgadmin.org/).

By default, Wasp uses port `5432`, or the next free port if `5432` is taken. With `--db-port <port>`, it fails if the requested port is unavailable:

```bash
wasp start --db-port 8080
```

##### Customising the dev database {#custom-database}

The Wasp development database uses the [PostgreSQL 18 Docker image](https://hub.docker.com/_/postgres/tags?name=18) by default, and will set up its data volumes according to their guidance.

All the commands above accept these options when starting a development database:

- `--db-port`: Specify the database port

- `--db-image`: Specify a custom Docker image
    
  Useful for PostgreSQL extensions or specific versions (for example, PostGIS, pgvector, etc.). 

- `--db-volume-mount-path`: Specify the volume mount path inside the container

  You only need to set this option if your custom `--db-image` is based on **PostgreSQL 17 or older** (check the `postgres:15` example below). 
  
  If the volume mount path is incorrect, the data won't be persisted in your development database.

Options apply only to the current invocation and are not remembered. Wasp rejects these options for SQLite or when `DATABASE_URL` is set. When a command uses an already running development database, Wasp warns and ignores the options.

Here are some examples of customising the development database:

```bash
# Use default PostgreSQL image:
wasp db start
# Same as:
wasp db start --db-image postgres:18

# Use PostgreSQL with PostGIS extension for geographic data:
wasp db start --db-image postgis/postgis:18-3.6

# Use PostgreSQL with pgvector extension for AI embeddings:
wasp db start --db-image pgvector/pgvector:pg18

# Use PostgreSQL version 15 (requires different volume path):
wasp db start --db-image postgres:15 --db-volume-mount-path /var/lib/postgresql/data
```

:::note

The custom Docker image you specify must use the `POSTGRES_DB`, `POSTGRES_USER`, and `POSTGRES_PASSWORD` environment variables when configuring the database. Wasp will use those values when connecting to the database. We recommend basing your image on the official [PostgreSQL Docker image](https://hub.docker.com/_/postgres), as it automatically uses these environment variables to set up the database name, user, and password.

:::

#### Connecting to an existing database

If you want to spin up your own dev database (or connect to an external one), you can tell Wasp about it using the `DATABASE_URL` environment variable. Wasp will use the value of `DATABASE_URL` as a connection string. It does not start or stop that database.

The easiest way to set the necessary `DATABASE_URL` environment variable is by adding it to the [.env.server](../../advanced/env-vars) file in the root dir of your Wasp project (if that file doesn't yet exist, create it):

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

2. Delete all the old migrations, since they are SQLite migrations and can't be used with PostgreSQL, as well as the SQLite database by running [`wasp clean`](../../advanced/cli#project-commands):

   ```bash
   rm -r migrations/
   wasp clean
   ```

3. Stop `wasp start` if it is running, then run `wasp db migrate-dev` to create and apply a new initial migration. Wasp starts the development database if needed. If you set `DATABASE_URL`, make sure that database is running first.

4. Run `wasp start` to start your app.

## Seeding the Database

**Database seeding** is a term used for populating the database with some initial data.

Seeding is most commonly used for:

1. Getting the development database into a state convenient for working and testing.
2. Initializing any database (`dev`, `staging`, or `prod`) with essential data it requires to operate.
   For example, populating the Currency table with default currencies, or the Country table with all available countries.

### Writing a Seed Function

You can define as many **seed functions** as you want in an array under the `db.seeds` field:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { devSeedSimple, prodSeed } from "./src/dbSeeds" with { type: "ref" }

export default app({
  name: "MyApp",
  // ...
  db: {
    seeds: [devSeedSimple, prodSeed],
  },
})
```

Each seed function must be an async function that takes one argument, `prisma`, which is a [Prisma Client](https://www.prisma.io/docs/concepts/components/prisma-client/crud) instance used to interact with the database.
This is the same Prisma Client instance that Wasp uses internally.

Since a seed function falls under server-side code, it can import other server-side functions. This is convenient because you might want to seed the database using Actions.

Here's an example of a seed function that imports an Action:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js
    import { createTask } from "./actions.js"
    import { sanitizeAndSerializeProviderData } from "wasp/server/auth"

    export const devSeedSimple = async (prisma) => {
      const user = await createUser(prisma, {
        username: "RiuTheDog",
        password: "bark1234",
      })

      await createTask(
        { description: "Chase the cat" },
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
                  providerName: "username",
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
    import { createTask } from "./actions.js"
    import type { DbSeedFn } from "wasp/server"
    import { sanitizeAndSerializeProviderData } from "wasp/server/auth"
    import type { AuthUser } from "wasp/auth"
    import type { PrismaClient } from "wasp/server"

    export const devSeedSimple: DbSeedFn = async (prisma) => {
      const user = await createUser(prisma, {
        username: "RiuTheDog",
        password: "bark1234",
      })

      await createTask(
        { description: "Chase the cat", isDone: false },
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
                  providerName: "username",
                  providerUserId: data.username,
                  providerData: await sanitizeAndSerializeProviderData<"username">({
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
To customize the client, define a function in the `db.prismaSetupFn` field that returns a Prisma Client instance.
This allows you to configure features like [logging](https://www.prisma.io/docs/orm/prisma-client/observability-and-logging/logging) or [client extensions](https://www.prisma.io/docs/orm/prisma-client/client-extensions):

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { setUpPrisma } from "./src/prisma" with { type: "ref" }

export default app({
  name: "MyApp",
  // ...
  db: {
    prismaSetupFn: setUpPrisma,
  },
})
```

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/prisma.js"
    import { PrismaClient } from "@prisma/client"

    export const setUpPrisma = () => {
      const prisma = new PrismaClient({
        log: ["query"],
      }).$extends({
        query: {
          task: {
            async findMany({ args, query }) {
              args.where = {
                ...args.where,
                description: { not: { contains: "hidden by setUpPrisma" } },
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
    ```ts title="src/prisma.ts"
    import { PrismaClient } from "@prisma/client"

    export const setUpPrisma = () => {
      const prisma = new PrismaClient({
        log: ["query"],
      }).$extends({
        query: {
          task: {
            async findMany({ args, query }) {
              args.where = {
                ...args.where,
                description: { not: { contains: "hidden by setUpPrisma" } },
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

<CardLink
  to="../../api/@wasp.sh/spec/interfaces/Db"
  kind="api"
  title="Db"
  description="All the options for the db field of the app spec."
/>

### CLI Commands for Seeding the Database

Use one of the following commands to run the seed functions:

- `wasp db seed`

  If you've only defined a single seed function, this command runs it. If you've defined multiple seed functions, it asks you to choose one interactively.

- `wasp db seed <seed-name>`

  This command runs the seed function with the specified name. Wasp derives this name from the imported function name you list in `db.seeds`.
  For example, to run the seed function `devSeedSimple` which was defined like this:

  ```ts title="main.wasp.ts"
  import { app } from "@wasp.sh/spec"
  import { devSeedSimple } from "./src/dbSeeds" with { type: "ref" }

  export default app({
    name: "MyApp",
    // ...
    db: {
      seeds: [
        // ...
        devSeedSimple,
      ],
    },
  })
  ```

  Use the following command:

  ```
  wasp db seed devSeedSimple
  ```
