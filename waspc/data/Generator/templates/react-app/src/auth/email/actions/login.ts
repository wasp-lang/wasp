import api, { handleApiError } from '../../../api'

export async function login(data: { email: string; password: string }): Promise<{ success: boolean }> {
    return api.post('/auth/email/login', data).catch(handleApiError);
}