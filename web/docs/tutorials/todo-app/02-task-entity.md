---
id: 02-task-entity
title: "Task entity"
---

import useBaseUrl from '@docusaurus/useBaseUrl';

[Entities](language/features.md#entity) are one of the very central concepts in Wasp, and they mainly play the role of data models.

Since our TodoApp is all about tasks, we will define a Task entity in Wasp:
```c title="main.wasp"
// ...

entity Task {=psl
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
psl=}
```

Since Wasp uses [Prisma](https://www.prisma.io) as a database, the definition of an entity comes down to defining a [Prisma model](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-schema/data-model/), using PSL (Prisma Schema Language) inside the `{=psl psl=}` tags.

After this change and before running `wasp start`, we first need to run:
```shell
wasp db migrate-dev
```
This instructs Prisma to create a new database schema migration (you'll see a new directory `migrations/` appeared in the root dir of our app) and apply it to the database.

To take a look at the database and the new `Task` schema, run:
```shell
wasp db studio
```

<img alt="Todo App - Db studio showing Task schema"
     src={useBaseUrl('img/todo-app-db-studio-task-entity.png')}
     style={{ border: "1px solid black" }}
/>

Click on the specific entity (we have only `Task` for now) and check out its fields! We don't have any data yet in our database, but we are about to change that.


