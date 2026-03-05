import { api, handleApiError } from 'wasp/client/api';
import { initSession } from '../../helpers/user';

// PUBLIC API
export async function login(data: { email: string; password: string }): Promise<void> {
    try {
        const response = await api.post('/auth/email/login', data);
        await initSession(response.data.sessionId);
    } catch (e) {
        throw handleApiError(e);
    }
}
