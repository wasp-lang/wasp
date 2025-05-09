import { deserialize, serialize } from 'superjson';
export type Payload = void | SuperJSONValue;
export type JSONValue = PrimitiveJSONValue | JSONArray | JSONObject;
export interface JSONObject {
    [key: string]: JSONValue;
}
type PrimitiveJSONValue = string | number | boolean | undefined | null;
export interface JSONArray extends Array<JSONValue> {
}
export type SerializableJSONValue = Set<SuperJSONValue> | Map<SuperJSONValue, SuperJSONValue> | undefined | bigint | Date | RegExp | Decimal;
export type SuperJSONValue = JSONValue | SerializableJSONValue | SuperJSONArray | SuperJSONObject;
export interface SuperJSONArray extends Array<SuperJSONValue> {
}
export interface SuperJSONObject {
    [key: string]: SuperJSONValue;
}
type Decimal = import("@prisma/client/runtime/library").Decimal;
declare const Decimal: any;
export { deserialize, serialize };
//# sourceMappingURL=serialization.d.ts.map