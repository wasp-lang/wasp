{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api';
import { initSession } from '../../../../core/auth/helpers/user.js';

// PUBLIC API
export async function login(data: { email: string; password: string }): Promise<void> {
    try {
        const response = await api.post('{= loginPath =}', data);
        await initSession(response.data.sessionId);
    } catch (e) {
        throw handleApiError(e);
    }
}
