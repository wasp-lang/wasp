import api, { handleApiError } from '../../../api'

export async function signup(data: { email: string; password: string }): Promise<{ success: boolean }> {
    return api.post('/auth/email/signup', data).catch(handleApiError);
}