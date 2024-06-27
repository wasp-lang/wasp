---
title: Prisma Schema File
---

import ImgWithCaption from '@site/blog/components/ImgWithCaption'

Wasp uses [Prisma](https://www.prisma.io/) to interact with the database. Prisma is a "Next-generation Node.js and TypeScript ORM" that provides a type-safe API for working with your database.

With Prisma, you define your application's data model in a `schema.prisma` file. 

In Wasp, the `schema.prisma` file is located in your project's root directory:

```c
.
├── main.wasp
...
// highlight-next-line
├── schema.prisma
├── src
├── tsconfig.json
└── vite.config.ts
```

 Wasp uses the `schema.prisma` file to understand your app's data model and generate the necessary code to interact with the database.

## Wasp file and Prisma schema file

Let's see how Wasp and Prisma files work together to define your application.

Here's an example `schema.prisma` file where we defined some database options and two models (User and Task) with a one-to-many relationship:

```prisma title="schema.prisma"
datasource db {
  provider = "postgresql"
  url      = env("DATABASE_URL")
}

generator client {
  provider = "prisma-client-js"
}

model User {
  id      Int        @id @default(autoincrement())
  tasks   Task[]
}

model Task {
  id          Int        @id @default(autoincrement())
  description String
  isDone      Boolean    @default(false)
  user        User       @relation(fields: [userId], references: [id])
  userId      Int
}
```

Wasp reads this `schema.prisma` file and extracts the info about your database models and database config:

<ImgWithCaption alt="Relationship between Wasp file and Prisma file" source="img/data-model/prisma_in_wasp.png" caption="Relationship between Wasp file and Prisma file"/>

Wasp uses the `datasource` block to know which database you want to use (PostgreSQL in this case) and some other options.

Wasp uses the `generator` block to generate the Prisma Client code that you can use in your application to interact with the database.

Finally, Prisma models represent Wasp Entities which can be then used in the `main.wasp` file:

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.14.0"
  },
  title: "My App",
}

...

// Using Wasp Entities in the Wasp file

query getTasks {
  fn: import { getTasks } from "@src/queries",
  // highlight-next-line
  entities: [Task]
}

job myJob {
  executor: PgBoss,
  perform: {
    fn: import { foo } from "@src/workers/bar"
  },
  // highlight-next-line
  entities: [Task],
}

api fooBar {
  fn: import { fooBar } from "@src/apis",
  // highlight-next-line
  entities: [Task],
  httpRoute: (GET, "/foo/bar/:email")
}

```

In the implementation of the `getTasks` query, `Task` is a Wasp Entity that corresponds to the `Task` model defined in the `schema.prisma` file. 

The same goes for the `myJob` job and `fooBar` API, where `Task` is used as an Entity.

## Wasp-specific Prisma configuration

Wasp mostly lets you use the Prisma schema file as you would in any other JS/TS project. However, there are some Wasp-specific rules you need to follow.

### The `datasource` block

```prisma title="schema.prisma"
datasource db {
  provider = "postgresql"
  url      = env("DATABASE_URL")
}
```

Wasp will take the `datasource` you write and use it as-is.

There are some rules you need to follow:
- You can only use `"postgresql"` or `"sqlite"` as the `provider` because Wasp only supports PostgreSQL and SQLite databases. 
- You must set the `url` field to `env("DATABASE_URL")` so that Wasp can work properly with your database.

### The `generator` blocks

```prisma title="schema.prisma"
generator client {
  provider = "prisma-client-js"
}
```

Wasp requires that there is a `generator` block with `provider = "prisma-client-js"` in the `schema.prisma` file.

You can add additional generators if you want to use them in your project.

### The `model` blocks

```prisma title="schema.prisma"
model User {
  id      Int        @id @default(autoincrement())
  tasks   Task[]
}

model Task {
  id          Int        @id @default(autoincrement())
  description String
  isDone      Boolean    @default(false)
  user        User       @relation(fields: [userId], references: [id])
  userId      Int
}
```

You can define your models in any way you like, if it's valid Prisma schema code, it will work with Wasp.

Some minor caveats to keep in mind:

- Wasp doesn't support `/// comment` syntax in the `schema.prisma` file. We are tracking it [here](https://github.com/wasp-lang/wasp/issues/2132), let us know if this is something you need.
- Wasp doesn't support MongoDB-specific [composite types](https://www.prisma.io/docs/orm/reference/prisma-schema-reference#type) in the `schema.prisma` file.

## Prisma preview features

Prisma is still in active development and some of its features are not yet stable. To use them, you have to enable them in the client `generator` block:

```prisma title="schema.prisma"
// ...

generator client {
  provider        = "prisma-client-js"
  // highlight-next-line
  previewFeatures = ["postgresqlExtensions"]
}
```

<small>

Read more about Prisma preview features in the [Prisma docs](https://www.prisma.io/docs/concepts/components/preview-features/client-preview-features).
</small>

### PostgreSQL Extensions

PostgreSQL supports [extensions](https://www.postgresql.org/docs/current/contrib.html) that add additional functionality to the database. For example, the [hstore](https://www.postgresql.org/docs/13/hstore.html) extension adds support for storing key-value pairs in a single column.

To use PostgreSQL extensions with Prisma, you have to enable them in the `datasource` block:

```prisma title="schema.prisma"
datasource db {
  provider   = "postgresql"
  url        = env("DATABASE_URL")
  // highlight-next-line
  extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
}

// ...
```

<small>

Read more about PostgreSQL configuration in Wasp in the [API Reference](#the-appdb-field).
</small>
