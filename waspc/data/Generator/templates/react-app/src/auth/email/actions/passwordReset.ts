{{={= =}=}}
import api, { handleApiError } from '../../../api';

export async function requestPasswordReset(data: { email: string; }): Promise<{ success: boolean }> {
    try {
        const response = await api.post('{= requestPasswordResetPath =}', data).catch(handleApiError);
        return response.data;
    } catch (e: unknown) {
        handleApiError(e);
    }
}

export async function resetPassword(data: { token: string; password: string; }): Promise<{ success: boolean }> {
    try {
        const response = await api.post('{= resetPasswordPath =}', data).catch(handleApiError);
        return response;
    } catch (e: unknown) {
        handleApiError(e);
    }
}
