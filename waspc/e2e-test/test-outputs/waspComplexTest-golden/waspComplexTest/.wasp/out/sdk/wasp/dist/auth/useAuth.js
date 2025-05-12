import { deserialize } from 'wasp/core/serialization';
import { useQuery, buildAndRegisterQuery } from 'wasp/client/operations';
import { api, handleApiError } from 'wasp/client/api';
import { HttpMethod } from 'wasp/client';
import { makeAuthUserIfPossible } from '../auth/user.js';
// PUBLIC API
export const getMe = createUserGetter();
// PUBLIC API
export default function useAuth() {
    return useQuery(getMe);
}
function createUserGetter() {
    const getMeRelativePath = 'auth/me';
    const getMeRoute = { method: HttpMethod.Get, path: `/${getMeRelativePath}` };
    const getMe = async () => {
        try {
            const response = await api.get(getMeRoute.path);
            const userData = deserialize(response.data);
            return makeAuthUserIfPossible(userData);
        }
        catch (error) {
            throw handleApiError(error);
        }
    };
    return buildAndRegisterQuery(getMe, {
        queryCacheKey: [getMeRelativePath],
        queryRoute: getMeRoute,
        entitiesUsed: ['User'],
    });
}
//# sourceMappingURL=useAuth.js.map