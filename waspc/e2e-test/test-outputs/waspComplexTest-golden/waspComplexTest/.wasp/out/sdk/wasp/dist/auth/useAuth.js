import { deserialize as superjsonDeserialize } from 'superjson';
import { useQuery, buildAndRegisterQuery } from 'wasp/client/operations';
import { api, handleApiError } from 'wasp/client/api';
import { HttpMethod } from 'wasp/client';
import { enrichAuthUser } from '../auth/user.js';
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
            return enrichAuthUser(superjsonDeserialize(response.data));
        }
        catch (error) {
            if (((_a = error.response) === null || _a === void 0 ? void 0 : _a.status) === 401) {
                return null;
            }
            else {
                handleApiError(error);
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