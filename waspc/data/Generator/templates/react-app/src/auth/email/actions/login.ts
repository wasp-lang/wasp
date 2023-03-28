{{={= =}=}}
import type { AxiosError } from 'axios';
import api, { handleApiError } from '../../../api'
import { initSession } from '../../helpers/user';

export async function login(data: { email: string; password: string }): Promise<void> {
    try {
        const response = await api.post('{= loginPath =}', data);
        await initSession(response.data.token);
    } catch (e: AxiosError) {
        handleApiError(e);
    }
}
