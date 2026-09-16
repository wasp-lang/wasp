import * as z from 'zod'

export const SessionResponseSchema = z.object({
    sessionId: z.string(),
});

export const SuccessResponseSchema = z.object({
    success: z.boolean(),
    reason: z.string().optional(),
});
