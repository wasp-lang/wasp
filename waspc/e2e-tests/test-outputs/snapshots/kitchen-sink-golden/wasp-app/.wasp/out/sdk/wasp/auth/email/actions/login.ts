import { api, handleApiError } from '../../../api/index.js';
import { initSession } from '@wasp.sh/lib-sdk-core/browser';
import { SessionResponseSchema } from '@wasp.sh/lib-sdk-core';

// PUBLIC API
export async function login(data: { email: string; password: string }): Promise<void> {
    try {
        const { sessionId } = await api.post('/auth/email/login', {
            json: data,
        }).json(SessionResponseSchema);
        await initSession(sessionId);
    } catch (e) {
        throw handleApiError(e);
    }
}
