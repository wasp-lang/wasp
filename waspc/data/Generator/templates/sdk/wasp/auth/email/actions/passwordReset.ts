{{={= =}=}}
import { api } from 'wasp/client/api';

// PUBLIC API
export async function requestPasswordReset(data: { email: string; }): Promise<{ success: boolean }> {
    return api.post('{= requestPasswordResetPath =}'.slice(1), {
        json: data,
    }).json();
}

// PUBLIC API
export async function resetPassword(data: { token: string; password: string; }): Promise<{ success: boolean }> {
    return api.post('{= resetPasswordPath =}'.slice(1), {
        json: data,
    }).json();
}
