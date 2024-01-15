---
title: 4. Database Entities
---

import useBaseUrl from '@docusaurus/useBaseUrl';

Entities are one of the most important concepts in Wasp and are how you define what gets stored in the database.

Since our Todo app is all about tasks, we will define a Task entity in the Wasp file:

```wasp title="main.wasp"
// ...

entity Task {=psl
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
psl=}
```

:::note
Wasp uses [Prisma](https://www.prisma.io) as a way to talk to the database. You define entities by defining [Prisma models](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-schema/data-model/) using the Prisma Schema Language (PSL) between the `{=psl psl=}` tags.

Read more in the [Entities](../data-model/entities) section of the docs.
:::

To update the database schema to include this entity, stop the `wasp start` process, if its running, and run:

```sh
wasp db migrate-dev
```

You'll need to do this any time you change an entity's definition. It instructs Prisma to create a new database migration and apply it to the database.

To take a look at the database and the new `Task` entity, run:

```sh
wasp db studio
```

This will open a new page in your browser to view and edit the data in your database.

<img alt="Todo App - Db studio showing Task schema"
src={useBaseUrl('img/todo-app-db-studio-task-entity.png')}
style={{ border: "1px solid black" }}
/>

Click on the `Task` entity and check out its fields! We don't have any data in our database yet, but we are about to change that.
