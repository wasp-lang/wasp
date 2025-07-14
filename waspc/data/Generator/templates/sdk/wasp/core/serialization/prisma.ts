import { Prisma } from "@prisma/client"
import { registerCustom } from "superjson"
import "./custom-register"

// This file is only added if there are (Prisma) entities in the AppSpec.

/*
  == ADDING SUPPORT FOR PRISMA DECIMAL TYPE ==
  We add support for the Decima Prisma type as listed here:
  https://www.prisma.io/docs/orm/prisma-client/special-fields-and-types
*/

/*
  == Why this strange declaration? ==

  You can usually get `Decimal` from Prisma in two ways:
  a. `import("@prisma/client").Prisma.Decimal`
  b. `import("@prisma/client/runtime/library").Decimal`

  The problem is that (a) is only available in the case that the Prisma schema is using Decimal
  somewhere, and doesn't exist at all otherwise, not even as `undefined`. And (b) is always
  available, but the code at `@prisma/client/runtime/library` fails when imported from the
  client (and this file can be imported from either client or server).

  What we do here is tell TypeScript that `Prisma` (option a) can have an optional `Decimal`
  property, and give it the `Decimal` type that we take from option (b). Importantly, we only
  do a type import from (b) so we don't trigger the runtime error when importing from the client.
*/
type DecimalClass = typeof import("@prisma/client/runtime/library").Decimal;
type DecimalInstance = InstanceType<DecimalClass>;
const Decimal = (Prisma as { Decimal?: DecimalClass }).Decimal;

/*
  And finally, if we have the `Decimal` type because the Prisma schema is using it,
  we register it as a custom type with SuperJSON.
  Based on https://github.com/flightcontrolhq/superjson/blob/v2.2.2/README.md#decimaljs--prismadecimal
*/
if (Decimal) {
  registerCustom<DecimalInstance, string>(
    {
      isApplicable: (v): v is DecimalInstance => Decimal.isDecimal(v),
      serialize: (v) => v.toJSON(),
      deserialize: (v) => new Decimal(v),
    },
    "prisma.decimal",
  );
}

// We add the `Decimal` to this interface so that it is registered as a custom serialization type
declare module "superjson" {
  interface WaspInternal_CustomSerializableJSONValue_Register {
    Decimal: DecimalInstance;
  }
}
