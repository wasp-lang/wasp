---
title: Entities
---

Entities are the foundation of your app's data model. In short, an Entity defines a model in your database.

Wasp uses the excellent [Prisma ORM](https://www.prisma.io/) to implement all database functionality and occasionally enhances it with a thin abstraction layer. This means that you use the `schema.prisma` file to define your database models and relationships. Wasp understands the Prisma schema file and picks up all the models you define there.

In your project you'll find a `schema.prisma` file in the root directory:

```
.
├── main.wasp
...
├── schema.prisma
├── src
├── tsconfig.json
└── vite.config.ts
```

Prisma uses the **_Prisma Schema Language (PSL)_**, a simple definition language explicitly created for defining models.
The language is declarative and very intuitive. We'll also go through an example later in the text, so there's no need to go and thoroughly learn it right away. Still, if you're curious, look no further than Prisma's official documentation:

- [Basic intro and examples](https://www.prisma.io/docs/orm/prisma-schema/overview)
- [A more exhaustive language specification](https://www.prisma.io/docs/orm/reference/prisma-schema-reference)

## Defining an Entity

A Prisma `model` declaration in the `schema.prisma` file represents a Wasp Entity.

Here's how you could define an Entity that represents a Task:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```prisma title="schema.prisma"
model Task {
  id          String  @id @default(uuid())
  description String
  isDone      Boolean @default(false)
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```prisma title="schema.prisma"
model Task {
  id          String  @id @default(uuid())
  description String
  isDone      Boolean @default(false)
}
```

</TabItem>
</Tabs>

The above Prisma `model` definition tells Wasp to create a table for storing Tasks where each task has three fields (i.e., the `tasks` table has three columns):

- `id` - A string value serving as a primary key. The database automatically generates it by generating a random unique ID.
- `description` - A string value for storing the task's description.
- `isDone` - A boolean value indicating the task's completion status. If you don't set it when creating a new task, the database sets it to `false` by default.

### Working with Entities

Let's see how you can define and work with Wasp Entities:

1. Create/update some Entities in the `schema.prisma` file.
2. Run `wasp db migrate-dev`. This command syncs the database model with the Entity definitions the `schema.prisma` file. It does this by creating migration scripts.
3. Migration scripts are automatically placed in the `migrations/` folder. Make sure to commit this folder into version control.
4. Use Wasp's JavasScript API to work with the database when implementing Operations (we'll cover this in detail when we talk about [operations](../data-model/operations/overview)).

#### Using Entities in Operations

Most of the time, you will be working with Entities within the context of [Operations (Queries & Actions)](../data-model/operations/overview). We'll see how that's done on the next page.

#### Using Entities directly

If you need more control, you can directly interact with Entities by importing and using the [Prisma Client](https://www.prisma.io/docs/concepts/components/prisma-client/crud). We recommend sticking with conventional Wasp-provided mechanisms, only resorting to directly using the Prisma client only if you need a feature Wasp doesn't provide.

You can only use the Prisma Client in your Wasp server code. You can import it like this:
<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js
import { prisma } from 'wasp/server'

prisma.task.create({
    description: "Read the Entities doc",
    isDone: true // almost :)
})
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts
import { prisma } from 'wasp/server'

prisma.task.create({
    description: "Read the Entities doc",
    isDone: true // almost :)
})
```

</TabItem>
</Tabs>

### Next steps

Now that we've seen how to define Entities that represent Wasp's core data model, we'll see how to make the most of them in other parts of Wasp. Keep reading to learn all about Wasp Operations!
