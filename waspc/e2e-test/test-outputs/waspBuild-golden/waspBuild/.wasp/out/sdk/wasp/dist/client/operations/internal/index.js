import { api, handleApiError } from 'wasp/client/api';
import { HttpMethod } from 'wasp/client';
import { serialize as superjsonSerialize, deserialize as superjsonDeserialize, } from 'superjson';
// PRIVATE API
export async function callOperation(operationRoute, args) {
    try {
        const superjsonArgs = superjsonSerialize(args);
        const response = await api.post(operationRoute.path, superjsonArgs);
        return superjsonDeserialize(response.data);
    }
    catch (error) {
        handleApiError(error);
    }
}
// PRIVATE API
export function makeOperationRoute(relativeOperationRoute) {
    return { method: HttpMethod.Post, path: `/${relativeOperationRoute}` };
}
//# sourceMappingURL=index.js.map