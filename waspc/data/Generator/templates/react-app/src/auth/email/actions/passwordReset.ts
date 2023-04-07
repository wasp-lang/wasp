{{={= =}=}}
import api, { handleApiError } from '../../../api';

export async function requestPasswordReset(data: { email: string; }): Promise<{ success: boolean }> {
    try {
        const response = await api.post('{= requestPasswordResetPath =}', data);
        return response.data;
    } catch (e) {
        handleApiError(e);
    }
}

export async function resetPassword(data: { token: string; password: string; }): Promise<{ success: boolean }> {
    try {
        const response = await api.post('{= resetPasswordPath =}', data);
        return response.data;
    } catch (e) {
        handleApiError(e);
    }
}
