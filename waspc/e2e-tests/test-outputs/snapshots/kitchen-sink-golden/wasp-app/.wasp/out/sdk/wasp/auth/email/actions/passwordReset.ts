import { api, handleApiError } from 'wasp/client/api';

// PUBLIC API
export async function requestPasswordReset(data: { email: string; }): Promise<{ success: boolean }> {
    try {
        const response = await api.post('/auth/email/request-password-reset', data);
        return response.data;
    } catch (e) {
        throw handleApiError(e);
    }
}

// PUBLIC API
export async function resetPassword(data: { token: string; password: string; }): Promise<{ success: boolean }> {
    try {
        const response = await api.post('/auth/email/reset-password', data);
        return response.data;
    } catch (e) {
        throw handleApiError(e);
    }
}
