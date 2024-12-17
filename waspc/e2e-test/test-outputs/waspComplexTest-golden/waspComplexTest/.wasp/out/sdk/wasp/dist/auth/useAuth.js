import { deserialize as superjsonDeserialize } from 'superjson';
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
        var _a;
        try {
            const response = await api.get(getMeRoute.path);
            const userData = superjsonDeserialize(response.data);
            return makeAuthUserIfPossible(userData);
        }
        catch (error) {
            if (((_a = error.response) === null || _a === void 0 ? void 0 : _a.status) === 401) {
                return null;
            }
            else {
                throw handleApiError(error);
            }
        }
    };
    return buildAndRegisterQuery(getMe, {
        queryCacheKey: [getMeRelativePath],
        queryRoute: getMeRoute,
        entitiesUsed: ['User'],
    });
}
//# sourceMappingURL=useAuth.js.map