{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api';
import { type UserEmailSignupFields } from '../../providers/types'

// PUBLIC API
export async function signup(data: { email: string; password: string } & UserEmailSignupFields): Promise<{ success: boolean }> {
    try {
        const response = await api.post('{= signupPath =}', data);
        return response.data;
    } catch (e) {
        throw handleApiError(e);
    }
}
