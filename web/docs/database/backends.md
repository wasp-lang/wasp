---
title: Databases
---

[Entities](/docs/database/entities.md), [Operations](/docs/database/operations.md) and [Automatic CRUD](/docs/database/crud.md) together make a high-level interface for working with your app's data. Still, all that data has to live somewhere, so let's see how Wasp deals with databases.

## Database configuration

You can tall Wasp which database to use in the `app` declaration's `db` field:

```wasp
app MyApp {
  title: "My app",
  // ...
  db: {
    system: PostgreSQL,
    seeds: [
      import devSeed from "@server/dbSeeds.js"
    ],
    prisma: {
      clientPreviewFeatures: ["extendedWhereUnique"]
    }
  }
}
```

`app.db` is a dictionary with following fields (all fields are optional):

- `system: DbSystem`

  The database system Wasp will use. It can be either `PostgreSQL` or `SQLite`.
  If not defined, or even if whole `db` field is not present, default value is `SQLite`.
  If you add/remove/modify `db` field, run `wasp db migrate-dev` to apply the changes.

- `seeds: [ServerImport]`

  Defines seed functions that you can use via `wasp db seed` to seed your database with initial data.
  Check out [Seeding](#seeding) section for more details.

- `prisma: [PrismaOptions]`

  Additional configuration for Prisma.
  It currently only supports a single field:

- `clientPreviewFeatures : string`

  Allows you to define [Prisma client preview features](https://www.prisma.io/docs/concepts/components/preview-features/client-preview-features).

### SQLite

Default database is `SQLite`, since it is great for getting started with a new project (needs no configuring), but it can be used only in development - once you want to deploy Wasp to production you will need to switch to `PostgreSQL` and stick with it.
Check below for more details on how to migrate from SQLite to PostgreSQL.

### PostgreSQL

When using `PostgreSQL` as your database (`app: { db: { system: PostgreSQL } }`), you will need to make sure you have a postgres database running during development (when running `wasp start` or doing `wasp db ...` commands).

### Using Wasp provided dev database

Wasp provides `wasp start db` command that starts the default dev db for you.

Your Wasp app will automatically connect to it once you have it running via `wasp start db`, no additional configuration is needed. This command relies on Docker being installed on your machine.

### Connecting to an existing database

If instead of using `wasp start db` you would rather spin up your own dev database or connect to some external database, you will need to provide Wasp with `DATABASE_URL` environment variable that Wasp will use to connect to it.

The easiest way to provide the needed `DATABASE_URL` environment variable is by adding it to the [.env.server](https://wasp-lang.dev/docs/language/features#env) file in the root dir of your Wasp project (if that file doesn't yet exist, create it).

You can also set it per command by doing `DATABASE_URL=<my-db-url> wasp ...` -> this can be useful if you want to run specific `wasp` command on a specific database.
Example: you could do `DATABASE_URL=<my-db-url> wasp db seed myProdSeed` to seed data for a fresh staging or production database.

### Migrating from SQLite to PostgreSQL

To run Wasp app in production, you will need to switch from `SQLite` to `PostgreSQL`.

1. Set `app.db.system` to `PostgreSQL`.
2. Delete old migrations, since they are SQLite migrations and can't be used with PostgreSQL: `rm -r migrations/`.
3. Run `wasp start db` to start your new db running (or check instructions above if you prefer using your own db). Leave it running, since we need it for the next step.
4. In a different terminal, run `wasp db migrate-dev` to apply new changes and create new, initial migration.
5. That is it, you are all done!

### Seeding

**Database seeding** is a term for populating database with some initial data.

Seeding is most commonly used for two following scenarios:

1.  To put development database into a state convenient for testing / playing with it.
2.  To initialize dev/staging/prod database with some essential data needed for it to be useful,
    for example default currencies in a Currency table.

#### Writing a seed function

Wasp enables you to define multiple **seed functions** via `app.db.seeds`:

```wasp
app MyApp {
  // ...
  db: {
    // ...
    seeds: [
      import { devSeedSimple } from "@server/dbSeeds.js",
      import { prodSeed } from "@server/dbSeeds.js"
    ]
  }
}
```

Each seed function is expected to be an async function that takes one argument, `prismaClient`, which is a [Prisma Client](https://www.prisma.io/docs/concepts/components/prisma-client/crud) instance that you can use to interact with the database.
This is the same instance of Prisma Client that Wasp uses internally, so you e.g. get password hashing for free.

Since a seed function is part of the server-side code, it can also import other server-side code, so you can and will normally want to import and use Actions to perform the seeding.

Example of a seed function that imports an Action (+ a helper function to create a user):
<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js
import { createTask } from "./actions.js";

export const devSeedSimple = async (prismaClient) => {
  const user = await createUser(prismaClient, {
    username: "RiuTheDog",
    password: "bark1234",
  });

  await createTask(
    { description: "Chase the cat" },
    { user, entities: { Task: prismaClient.task } }
  );
};

async function createUser(prismaClient, data) {
  const { password, ...newUser } = await prismaClient.user.create({ data });
  return newUser;
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts
import { createTask } from "./actions.js";
import { User } from "@wasp/entities";
import { PrismaClient } from "@prisma/client";

type SanitizedUser = Omit<User, "password">;

export const devSeedSimple = async (prismaClient: PrismaClient) => {
  const user = await createUser(prismaClient, {
    username: "RiuTheDog",
    password: "bark1234",
  });

  await createTask(
    { description: "Chase the cat", isDone: false },
    { user, entities: { Task: prismaClient.task } }
  );
};

async function createUser(
  prismaClient: PrismaClient,
  data: Pick<User, "username" | "password">
): Promise<SanitizedUser> {
  const { password, ...newUser } = await prismaClient.user.create({ data });
  return newUser;
}
```

</TabItem>
</Tabs>

#### Running seed functions

- `wasp db seed`: If you have just one seed function, it will run it. If you have multiple, it will interactively ask you to choose one to run.

- `wasp db seed <seed-name>`: It will run the seed function with the specified name, where the name is the identifier you used in its `import` expression in the `app.db.seeds` list. Example: `wasp db seed devSeedSimple`.

:::tip
Often you will want to call `wasp db seed` right after you ran `wasp db reset`: first you empty your database, then you fill it with some initial data.
:::
