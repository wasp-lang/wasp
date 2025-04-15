export type Payload = void | SuperJSONValue;
export type JSONValue = PrimitiveJSONValue | JSONArray | JSONObject;
export interface JSONObject {
    [key: string]: JSONValue;
}
type PrimitiveJSONValue = string | number | boolean | undefined | null;
export interface JSONArray extends Array<JSONValue> {
}
export type SerializableJSONValue = symbol | Set<SuperJSONValue> | Map<SuperJSONValue, SuperJSONValue> | undefined | bigint | Date | RegExp;
export type SuperJSONValue = JSONValue | SerializableJSONValue | SuperJSONArray | SuperJSONObject;
export interface SuperJSONArray extends Array<SuperJSONValue> {
}
export interface SuperJSONObject {
    [key: string]: SuperJSONValue;
}
export {};
