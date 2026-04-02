import { api } from 'wasp/client/api';
import { initSession } from '../../helpers/user';
import { SessionResponseSchema } from '../../responseSchemas';

// PUBLIC API
export async function login(data: { email: string; password: string }): Promise<void> {
    const { sessionId } = await api.post('/auth/email/login', {
        json: data,
    }).json(SessionResponseSchema);
    await initSession(sessionId);
}
