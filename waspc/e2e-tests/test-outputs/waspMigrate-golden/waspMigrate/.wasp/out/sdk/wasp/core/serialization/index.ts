import { deserialize, serialize } from "superjson"
import { CustomSerializableJSONValue } from "./custom-register"

import "./prisma"

export type Payload = void | SuperJSONValue;

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
export type JSONValue = PrimitiveJSONValue | JSONArray | JSONObject;

export interface JSONObject {
  [key: string]: JSONValue;
}

type PrimitiveJSONValue = string | number | boolean | undefined | null;

export interface JSONArray extends Array<JSONValue> {}

// Removed `Symbol` since we don't explicitly register any symbol to be serialized
// Added custom types from `CustomSerializableJSONValue`
export type SerializableJSONValue =
  | Set<SuperJSONValue>
  | Map<SuperJSONValue, SuperJSONValue>
  | undefined
  | bigint
  | Date
  | RegExp
  | CustomSerializableJSONValue;

// Here's where we excluded `ClassInstance` (which was `any`) from the union.
export type SuperJSONValue =
  | JSONValue
  | SerializableJSONValue
  | SuperJSONArray
  | SuperJSONObject;

export interface SuperJSONArray extends Array<SuperJSONValue> {}

export interface SuperJSONObject {
  [key: string]: SuperJSONValue;
}

export { deserialize, serialize }

