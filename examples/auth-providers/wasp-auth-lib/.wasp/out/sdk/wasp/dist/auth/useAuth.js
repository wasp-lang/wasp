import { deserialize } from '../core/serialization/index.js';
import { useQuery, buildAndRegisterQuery } from '../client/operations/index.js';
import { api, handleApiError } from '../api/index.js';
import { HttpMethod } from '../client/index.js';
import { makeAuthUserIfPossible } from './user.js';
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
            const json = await api.get(getMeRoute.path).json();
            const userData = deserialize(json);
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