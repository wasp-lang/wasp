import { api, handleApiError } from 'wasp/client/api';
import { SuccessResponseSchema } from '../../responseSchemas';

// PUBLIC API
export async function requestPasswordReset(data: { email: string; }): Promise<{ success: boolean }> {
    try {
        const { success } = await api.post('/auth/email/request-password-reset', {
            json: data,
        }).json(SuccessResponseSchema);
        return { success };
    } catch (e) {
        throw handleApiError(e);
    }
}

// PUBLIC API
export async function resetPassword(data: { token: string; password: string; }): Promise<{ success: boolean }> {
    try {
        const { success } = await api.post('/auth/email/reset-password', {
            json: data,
        }).json(SuccessResponseSchema);
        return { success };
    } catch (e) {
        throw handleApiError(e);
    }
}
