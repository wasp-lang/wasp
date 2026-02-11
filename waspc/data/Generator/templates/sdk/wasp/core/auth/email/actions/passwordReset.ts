{{={= =}=}}
import { api, handleApiError } from '../../../api/index.js';

// PUBLIC API
export async function requestPasswordReset(data: { email: string; }): Promise<{ success: boolean }> {
    try {
        const response = await api.post('{= requestPasswordResetPath =}', data);
        return response.data;
    } catch (e) {
        throw handleApiError(e);
    }
}

// PUBLIC API
export async function resetPassword(data: { token: string; password: string; }): Promise<{ success: boolean }> {
    try {
        const response = await api.post('{= resetPasswordPath =}', data);
        return response.data;
    } catch (e) {
        throw handleApiError(e);
    }
}
