import { Prisma } from '@prisma/client'
import type { Decimal as DecimalType } from "@prisma/client/runtime/library"
import { deserialize, registerCustom, serialize } from 'superjson'

// We add support for the Decima Prisma type
// as listed here https://www.prisma.io/docs/orm/prisma-client/special-fields-and-types

// We can't import `Decimal` from `@prisma/client/runtime/library`
// directly because it imports some server-only stuff.
// But the instance in `Prisma` might not be there if the schema doesn't
// have a Decimal property somewhere.
// We do this trick merging the type from one place and the maybe
// existing instance from another.
const Decimal = (Prisma as { Decimal?: typeof DecimalType }).Decimal;
type Decimal = DecimalType;

if (Decimal) {
  // Based on https://github.com/flightcontrolhq/superjson/blob/v2.2.2/README.md#decimaljs--prismadecimal
  registerCustom<Decimal, string>(
    {
      isApplicable: (v): v is Decimal => Decimal.isDecimal(v),
      serialize: (v) => v.toJSON(),
      deserialize: (v) => new Decimal(v),
    },
    "prisma.decimal"
  );
}

export type Payload = void | SuperJSONValue

// The part below was copied from SuperJSON and slightly modified:
// https://github.com/blitz-js/superjson/blob/ae7dbcefe5d3ece5b04be0c6afe6b40f3a44a22a/src/types.ts
//
// We couldn't use SuperJSON's types directly because:
//   1. They aren't exported publicly.
//   2. They have a werid quirk that turns `SuperJSONValue` into `any`.
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

type SerializableJSONValue =
  | Symbol
  | Set<SuperJSONValue>
  | Map<SuperJSONValue, SuperJSONValue>
  | undefined
  | bigint
  | Date
  | RegExp
  | Decimal

// Here's where we excluded `ClassInstance` (which was `any`) from the union.
type SuperJSONValue =
  | JSONValue
  | SerializableJSONValue
  | SuperJSONArray
  | SuperJSONObject

interface SuperJSONArray extends Array<SuperJSONValue> {}

interface SuperJSONObject {
  [key: string]: SuperJSONValue
}

export { deserialize, serialize }

