{{={= =}=}}
import { api, handleApiError } from '../../../api/index.js';
import { initSession } from '@wasp/lib-sdk-core/browser';
import { queryClient } from '../../../client/operations/queryClient.js';
import { SessionResponseSchema } from '@wasp/lib-sdk-core';

// PUBLIC API
export async function login(data: { email: string; password: string }): Promise<void> {
    try {
        const { sessionId } = await api.post('{= loginPath =}', {
            json: data,
        }).json(SessionResponseSchema);
        initSession(queryClient, sessionId);
    } catch (e) {
        throw handleApiError(e);
    }
}
