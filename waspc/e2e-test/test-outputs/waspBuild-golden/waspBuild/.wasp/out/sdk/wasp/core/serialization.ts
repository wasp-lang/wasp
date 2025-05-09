import { Prisma } from '@prisma/client'
import { deserialize, registerCustom, serialize } from 'superjson'

export type Payload = void | SuperJSONValue

// The part below was copied from SuperJSON and slightly modified:
// https://github.com/blitz-js/superjson/blob/ae7dbcefe5d3ece5b04be0c6afe6b40f3a44a22a/src/types.ts
//
// We couldn't use SuperJSON's types directly because:
//   1. They aren't exported publicly.
//   2. They have a weird quirk that turns `SuperJSONValue` into `any`.
//      See why here:
//      https://github.com/blitz-js/superjson/pull/36#issuecomment-669239876
//
// We changed the code as little as possible to make future comparisons easier.
export type JSONValue = PrimitiveJSONValue | JSONArray | JSONObject

export interface JSONObject {
  [key: string]: JSONValue
}

type PrimitiveJSONValue = string | number | boolean | undefined | null

export interface JSONArray extends Array<JSONValue> {}

// Added the `Decimal` type to the union (see below)
// Removed `Symbol` since we don't explicitly register any symbol to be serialized
export type SerializableJSONValue =
  | Set<SuperJSONValue>
  | Map<SuperJSONValue, SuperJSONValue>
  | undefined
  | bigint
  | Date
  | RegExp
  | Decimal

// Here's where we excluded `ClassInstance` (which was `any`) from the union.
export type SuperJSONValue =
  | JSONValue
  | SerializableJSONValue
  | SuperJSONArray
  | SuperJSONObject

export interface SuperJSONArray extends Array<SuperJSONValue> {}

export interface SuperJSONObject {
  [key: string]: SuperJSONValue
}

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
type Decimal = import("@prisma/client/runtime/library").Decimal
const Decimal = (Prisma as { Decimal?: typeof Decimal }).Decimal;

/*
  And finally, if we have the `Decimal` type because the Prisma schema is using it,
  we register it as a custom type with SuperJSON.
  Based on https://github.com/flightcontrolhq/superjson/blob/v2.2.2/README.md#decimaljs--prismadecimal
*/
if (Decimal) {
  registerCustom<Decimal, string>(
    {
      isApplicable: (v): v is Decimal => Decimal.isDecimal(v),
      serialize: (v) => v.toJSON(),
      deserialize: (v) => new Decimal(v),
    },
    "prisma.decimal"
  );
}

export { deserialize, serialize }

