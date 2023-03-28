import api, { handleApiError } from '../../../api'
import { initSession } from '../../helpers/user';

export async function login(data: { email: string; password: string }): Promise<void> {
    const response = await api.post('/auth/email/login', data)
        .catch(handleApiError);
    await initSession(response.data.token);
}
