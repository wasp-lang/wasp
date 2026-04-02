{{={= =}=}}
import { api } from 'wasp/client/api';
import { SuccessResponseSchema } from '../../responseSchemas';

// PUBLIC API
export async function requestPasswordReset(data: { email: string; }): Promise<{ success: boolean }> {
    const { success } = await api.post('{= requestPasswordResetPath =}', {
        json: data,
    }).json(SuccessResponseSchema);
    return { success };
}

// PUBLIC API
export async function resetPassword(data: { token: string; password: string; }): Promise<{ success: boolean }> {
    const { success } = await api.post('{= resetPasswordPath =}', {
        json: data,
    }).json(SuccessResponseSchema);
    return { success };
}
