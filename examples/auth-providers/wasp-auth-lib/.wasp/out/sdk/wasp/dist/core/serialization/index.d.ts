import { deserialize, serialize } from "superjson";
import { CustomSerializableJSONValue } from "./custom-register";
import "./prisma";
export type Payload = void | SuperJSONValue;
export type JSONValue = PrimitiveJSONValue | JSONArray | JSONObject;
export interface JSONObject {
    [key: string]: JSONValue;
}
type PrimitiveJSONValue = string | number | boolean | undefined | null;
export interface JSONArray extends Array<JSONValue> {
}
export type SerializableJSONValue = Set<SuperJSONValue> | Map<SuperJSONValue, SuperJSONValue> | undefined | bigint | Date | RegExp | CustomSerializableJSONValue;
export type SuperJSONValue = JSONValue | SerializableJSONValue | SuperJSONArray | SuperJSONObject;
export interface SuperJSONArray extends Array<SuperJSONValue> {
}
export interface SuperJSONObject {
    [key: string]: SuperJSONValue;
}
export { deserialize, serialize };
//# sourceMappingURL=index.d.ts.map