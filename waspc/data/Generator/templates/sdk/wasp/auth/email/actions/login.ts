{{={= =}=}}
import { api } from 'wasp/client/api';
import { initSession } from '../../helpers/user';

// PUBLIC API
export async function login(data: { email: string; password: string }): Promise<void> {
    const json = await api.post('{= loginPath =}', {
        json: data,
    }).json<{ sessionId: string }>();
    await initSession(json.sessionId);
}
