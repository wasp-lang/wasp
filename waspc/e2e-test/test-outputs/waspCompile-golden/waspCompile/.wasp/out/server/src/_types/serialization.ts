export type Payload = void | SuperJSONValue;

type PrimitiveJSONValue = string | number | boolean | undefined | null;

type JSONValue = PrimitiveJSONValue | JSONArray | JSONObject;

interface JSONArray extends Array<JSONValue> {
}

interface JSONObject {
    [key: string]: JSONValue;
}

type SerializableJSONValue = Symbol | Set<SuperJSONValue> | Map<SuperJSONValue, SuperJSONValue> | undefined | bigint | Date | RegExp;

type SuperJSONValue = JSONValue | SerializableJSONValue | SuperJSONArray | SuperJSONObject;

interface SuperJSONArray extends Array<SuperJSONValue> {
}

interface SuperJSONObject {
    [key: string]: SuperJSONValue;
}
