import { deserialize as superjsonDeserialize } from 'superjson';
import { useQuery, addMetadataToQuery } from 'wasp/client/operations';
import { api, handleApiError } from 'wasp/client/api';
import { HttpMethod } from 'wasp/client';
// PUBLIC API
export const getMe = createUserGetter();
// PUBLIC API
export default function useAuth(queryFnArgs, config) {
    return useQuery(getMe, queryFnArgs, config);
}
function createUserGetter() {
    const getMeRelativePath = 'auth/me';
    const getMeRoute = { method: HttpMethod.Get, path: `/${getMeRelativePath}` };
    async function getMe() {
        var _a;
        try {
            const response = await api.get(getMeRoute.path);
            return superjsonDeserialize(response.data);
        }
        catch (error) {
            if (((_a = error.response) === null || _a === void 0 ? void 0 : _a.status) === 401) {
                return null;
            }
            else {
                handleApiError(error);
            }
        }
    }
    addMetadataToQuery(getMe, {
        relativeQueryPath: getMeRelativePath,
        queryRoute: getMeRoute,
        entitiesUsed: ['User'],
    });
    return getMe;
}
//# sourceMappingURL=useAuth.js.map