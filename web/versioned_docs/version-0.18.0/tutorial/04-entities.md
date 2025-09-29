---
title: 4. Database Entities
---

import useBaseUrl from '@docusaurus/useBaseUrl';

Entities are one of the most important concepts in Wasp and are how you define what gets stored in the database.

Wasp uses Prisma to talk to the database, and you define Entities by defining Prisma models in the `schema.prisma` file.

Since our Todo app is all about tasks, we'll define a Task entity by adding a Task model in the `schema.prisma` file:

```prisma title="schema.prisma"
// ...

model Task {
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
}
```

:::note
Read more about how Wasp Entities work in the [Entities](../data-model/entities.md) section or how Wasp uses the `schema.prisma` file in the [Prisma Schema File](../data-model/prisma-file.md) section.
:::

To update the database schema to include this entity, stop the `wasp start` process, if it's running, and run:

```sh
wasp db migrate-dev
```

You'll need to do this any time you change an entity's definition. It instructs Prisma to create a new database migration and apply it to the database.

To take a look at the database and the new `Task` entity, run:

```sh
wasp db studio
```

This will open a new page in your browser to view and edit the data in your database.

<img alt="Todo App - Db studio showing Task schema" src={useBaseUrl('img/todo-app-db-studio-task-entity.png')} className="tutorial-image" />

Click on the `Task` entity and check out its fields! We don't have any data in our database yet, but we are about to change that.
