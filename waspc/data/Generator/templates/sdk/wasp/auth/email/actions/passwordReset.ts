{{={= =}=}}
import { api, handleApiError } from '../../../api/index.js';
import { SuccessResponseSchema } from '../../responseSchemas';
import { browserAppDelivery } from '../../../client/index.js';

// PUBLIC API
export async function requestPasswordReset(data: { email: string; }): Promise<{ success: boolean }> {
    try {
        const { success } = await api.post(browserAppDelivery.waspApiPath('{= requestPasswordResetPath =}'), {
            json: data,
        }).json(SuccessResponseSchema);
        return { success };
    } catch (e) {
        throw handleApiError(e);
    }
}

// PUBLIC API
export async function resetPassword(data: { token: string; password: string; }): Promise<{ success: boolean }> {
    try {
        const { success } = await api.post(browserAppDelivery.waspApiPath('{= resetPasswordPath =}'), {
            json: data,
        }).json(SuccessResponseSchema);
        return { success };
    } catch (e) {
        throw handleApiError(e);
    }
}
