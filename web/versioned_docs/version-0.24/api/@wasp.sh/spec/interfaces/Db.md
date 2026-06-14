# Interface: Db

Database configuration.

See [Databases](https://wasp.sh/docs/data-model/databases) for seeding and
Prisma client customization.

## Example

```ts
import { app } from "@wasp.sh/spec"
import devSeed from "./src/dbSeeds" with { type: "ref" }
import { setUpPrisma } from "./src/prisma" with { type: "ref" }

export default app({
  // ...
  db: {
    seeds: [devSeed],
    prismaSetupFn: setUpPrisma,
  },
})
```

## Properties

### prismaSetupFn?

> `optional` **prismaSetupFn?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Function that sets up and returns a configured Prisma Client instance.
Use this to add Prisma logging or client extensions.

See Prisma's [logging](https://www.prisma.io/docs/orm/prisma-client/observability-and-logging/logging)
and [client extensions](https://www.prisma.io/docs/orm/prisma-client/client-extensions)
docs.

#### Example

```ts
import { PrismaClient } from "@prisma/client"

export const setUpPrisma = () => {
  const prisma = new PrismaClient({
    log: ["query", "info", "warn", "error"],
  })

  return prisma
}
```

***

### seeds?

> `optional` **seeds?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>[]

Async functions runnable with `wasp db seed [name]` to populate the
database with initial data. Each function receives Wasp's Prisma Client;
the name passed to `wasp db seed` matches the function's identifier in the
import.

See [Seeding the Database](https://wasp.sh/docs/data-model/databases#seeding-the-database).
