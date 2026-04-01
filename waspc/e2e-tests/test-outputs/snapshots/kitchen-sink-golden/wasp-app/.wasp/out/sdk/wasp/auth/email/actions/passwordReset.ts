import { api } from 'wasp/client/api';

// PUBLIC API
export async function requestPasswordReset(data: { email: string; }): Promise<{ success: boolean }> {
    return api.post('/auth/email/request-password-reset'.slice(1), {
        json: data,
    }).json();
}

// PUBLIC API
export async function resetPassword(data: { token: string; password: string; }): Promise<{ success: boolean }> {
    return api.post('/auth/email/reset-password'.slice(1), {
        json: data,
    }).json();
}
