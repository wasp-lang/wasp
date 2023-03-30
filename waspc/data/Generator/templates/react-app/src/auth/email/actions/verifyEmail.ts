{{={= =}=}}
import type { AxiosError } from 'axios';
import api, { handleApiError } from '../../../api';

export async function verifyEmail(data: { token: string; }): Promise<{ success: boolean; reason?: string; }> {
    try {
        const response = await api.post('{= verifyEmailPath =}', data);
        return response.data;
    } catch (e: unknown) {
        handleApiError(e);
    }
}