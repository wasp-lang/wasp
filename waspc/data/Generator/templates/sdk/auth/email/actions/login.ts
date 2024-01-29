{{={= =}=}}
import api, { handleApiError } from 'wasp/api';
import { initSession } from '../../helpers/user';

export async function login(data: { email: string; password: string }): Promise<void> {
    try {
        const response = await api.post('{= loginPath =}', data);
        await initSession(response.data.sessionId);
    } catch (e) {
        handleApiError(e);
    }
}
