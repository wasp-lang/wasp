import { api } from 'wasp/client/api';
import { SuccessResponseSchema } from '../../responseSchemas';

// PUBLIC API
export async function requestPasswordReset(data: { email: string; }): Promise<{ success: boolean }> {
    const json = await api.post('/auth/email/request-password-reset', {
        json: data,
    }).json();
    const { success } = SuccessResponseSchema.parse(json);
    return { success };
}

// PUBLIC API
export async function resetPassword(data: { token: string; password: string; }): Promise<{ success: boolean }> {
    const json = await api.post('/auth/email/reset-password', {
        json: data,
    }).json();
    const { success } = SuccessResponseSchema.parse(json);
    return { success };
}
