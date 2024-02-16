export type Payload = void | SuperJSONValue;
export type JSONValue = PrimitiveJSONValue | JSONArray | JSONObject;
export interface JSONObject {
    [key: string]: JSONValue;
}
type PrimitiveJSONValue = string | number | boolean | undefined | null;
export interface JSONArray extends Array<JSONValue> {
}
type SerializableJSONValue = Symbol | Set<SuperJSONValue> | Map<SuperJSONValue, SuperJSONValue> | undefined | bigint | Date | RegExp;
type SuperJSONValue = JSONValue | SerializableJSONValue | SuperJSONArray | SuperJSONObject;
interface SuperJSONArray extends Array<SuperJSONValue> {
}
interface SuperJSONObject {
    [key: string]: SuperJSONValue;
}
export {};
