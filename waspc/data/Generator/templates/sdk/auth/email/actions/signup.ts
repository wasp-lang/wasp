{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api';

export async function signup(data: { email: string; password: string }): Promise<{ success: boolean }> {
    try {
        const response = await api.post('{= signupPath =}', data);
        return response.data;
    } catch (e) {
        handleApiError(e);
    }
}
