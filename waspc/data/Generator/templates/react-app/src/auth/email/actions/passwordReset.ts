import api, { handleApiError } from '../../../api'

export async function requestPasswordReset(data: { email: string; }): Promise<{ success: boolean }> {
    return api.post('/auth/email/request-password-reset', data).catch(handleApiError);
}

export async function resetPassword(data: { token: string; newPassword: string; }): Promise<{ success: boolean }> {
    return api.post('/auth/email/reset-password', data).catch(handleApiError);
}
