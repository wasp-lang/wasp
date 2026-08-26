{{={= =}=}}
import { api, handleApiError } from '../../../api/index.js';
import { initSession } from '../../helpers/user';
import { SessionResponseSchema } from '../../responseSchemas';
import { browserAppDelivery } from '../../../client/index.js';

// PUBLIC API
export async function login(data: { email: string; password: string }): Promise<void> {
    try {
        const { sessionId } = await api.post(browserAppDelivery.waspApiPath('{= loginPath =}'), {
            json: data,
        }).json(SessionResponseSchema);
        await initSession(sessionId);
    } catch (e) {
        throw handleApiError(e);
    }
}
