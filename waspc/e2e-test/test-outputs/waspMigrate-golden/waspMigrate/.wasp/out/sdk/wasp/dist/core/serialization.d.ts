import { Prisma } from '@prisma/client';
import type { Decimal as DecimalType } from "@prisma/client/runtime/library";
declare const Decimal: typeof Prisma.Decimal | undefined;
type Decimal = DecimalType;
export type Payload = void | SuperJSONValue;
export type JSONValue = PrimitiveJSONValue | JSONArray | JSONObject;
export interface JSONObject {
    [key: string]: JSONValue;
}
type PrimitiveJSONValue = string | number | boolean | undefined | null;
export interface JSONArray extends Array<JSONValue> {
}
type SerializableJSONValue = Symbol | Set<SuperJSONValue> | Map<SuperJSONValue, SuperJSONValue> | undefined | bigint | Date | RegExp | Decimal;
type SuperJSONValue = JSONValue | SerializableJSONValue | SuperJSONArray | SuperJSONObject;
interface SuperJSONArray extends Array<SuperJSONValue> {
}
interface SuperJSONObject {
    [key: string]: SuperJSONValue;
}
export declare const serialize: (object: import("superjson/dist/types").SuperJSONValue) => import("superjson").SuperJSONResult;
export declare const deserialize: <T = unknown>(payload: import("superjson").SuperJSONResult) => T;
export {};
//# sourceMappingURL=serialization.d.ts.map