{{={= =}=}}
import type { AxiosError } from 'axios';
import api, { handleApiError } from '../../../api';

export async function requestPasswordReset(data: { email: string; }): Promise<{ success: boolean }> {
    try {
        const response = api.post('{= requestPasswordResetPath =}', data).catch(handleApiError);
        return response.data;
    } catch (e: unknown) {
        handleApiError(e);
    }
}

export function resetPassword(data: { token: string; newPassword: string; }): Promise<{ success: boolean }> {
    try {
        const response = api.post('{= resetPasswordPath =}', data).catch(handleApiError);
        return response;
    } catch (e: unknown) {
        handleApiError(e);
    }
}
