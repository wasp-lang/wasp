import { Prisma } from '@prisma/client';
import SuperJSON from 'superjson';
// We add support for the Decima Prisma type
// as listed here https://www.prisma.io/docs/orm/prisma-client/special-fields-and-types
// We can't import `Decimal` from `@prisma/client/runtime/library`
// directly because it imports some server-only stuff.
// But the instance in `Prisma` might not be there if the schema doesn't
// have a Decimal property somewhere.
// We do this trick merging the type from one place and the maybe
// existing instance from another.
const Decimal = Prisma.Decimal;
if (Decimal) {
    // Based on https://github.com/flightcontrolhq/superjson#decimaljs--prismadecimal
    SuperJSON.registerCustom({
        isApplicable: (v) => Decimal.isDecimal(v),
        serialize: (v) => v.toJSON(),
        deserialize: (v) => new Decimal(v),
    }, "prisma.decimal");
}
export const serialize = SuperJSON.serialize.bind(SuperJSON);
export const deserialize = SuperJSON.deserialize.bind(SuperJSON);
//# sourceMappingURL=serialization.js.map