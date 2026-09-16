import { Prisma } from "@prisma/client";
import { registerCustom } from "superjson";
import "./custom-register";
const Decimal = Prisma.Decimal;
/*
  And finally, if we have the `Decimal` type because the Prisma schema is using it,
  we register it as a custom type with SuperJSON.
  Based on https://github.com/flightcontrolhq/superjson/blob/v2.2.2/README.md#decimaljs--prismadecimal
*/
if (Decimal) {
    registerCustom({
        isApplicable: (v) => Decimal.isDecimal(v),
        serialize: (v) => v.toJSON(),
        deserialize: (v) => new Decimal(v),
    }, "prisma.decimal");
}
//# sourceMappingURL=prisma.js.map