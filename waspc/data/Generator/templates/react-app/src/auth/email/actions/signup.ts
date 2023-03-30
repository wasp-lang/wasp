{{={= =}=}}
import type { AxiosError } from 'axios';
import api, { handleApiError } from '../../../api';

export async function signup(data: { username: string; password: string }): Promise<{ success: boolean }> {
    try {
        const response = await api.post('{= signupPath =}', data);
        return response.data;
    } catch (e: unknown) {
        handleApiError(e);
    }
}