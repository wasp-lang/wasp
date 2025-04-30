import { api, handleApiError } from 'wasp/client/api';
import { HttpMethod } from 'wasp/client';
import { serialize, deserialize } from 'wasp/core/serialization';
// PRIVATE API
export async function callOperation(operationRoute, args) {
    try {
        const serializedArgs = serialize(args);
        const response = await api.post(operationRoute.path, serializedArgs);
        return deserialize(response.data);
    }
    catch (error) {
        throw handleApiError(error);
    }
}
// PRIVATE API
export function makeOperationRoute(relativeOperationRoute) {
    return { method: HttpMethod.Post, path: `/${relativeOperationRoute}` };
}
//# sourceMappingURL=index.js.map