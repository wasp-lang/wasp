{{={= =}=}}
import { deserialize, serialize } from "superjson"


{=# entitiesExist =}
import "./prisma"
{=/ entitiesExist =}

export { type JSONArray, type JSONObject, type JSONValue, type Payload, type SerializableJSONValue, type SuperJSONArray, type SuperJSONObject, type SuperJSONValue } from '@wasp/sdk-core'

export { deserialize, serialize }
