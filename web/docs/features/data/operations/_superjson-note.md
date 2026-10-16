import { ShowForTs } from '@site/src/components/TsJsHelpers';

:::info Payload constraints
Wasp uses [superjson](https://github.com/flightcontrolhq/superjson) under the hood.
This means you're not limited to only sending and receiving JSON payloads.

Wasp will automatically handle the serialization and deserialization
[for all the data types that superjson supports](https://github.com/flightcontrolhq/superjson#decimaljs--prismadecimal:~:text=Superjson%20supports%20many%20extra%20types)
(like `biging`, `Date`, `Map`, `Set`, etc.), and for
[Prisma.Decimal](https://www.prisma.io/docs/orm/prisma-client/special-fields-and-types#working-with-decimal).

<ShowForTs>
  As long as you're annotating your Operations with the correct automatically generated types, TypeScript ensures your payloads are valid (i.e., Wasp knows how to serialize and deserialize them).
</ShowForTs>

:::
