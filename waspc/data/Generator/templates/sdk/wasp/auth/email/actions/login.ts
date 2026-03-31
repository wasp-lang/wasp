{{={= =}=}}
import { api } from 'wasp/client/api';
import { initSession } from '../../helpers/user';

// PUBLIC API
export async function login(data: { email: string; password: string }): Promise<void> {
    const response = await api('{= loginPath =}', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
    });
    const json = await response.json();
    await initSession(json.sessionId);
}
