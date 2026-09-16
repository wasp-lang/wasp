import * as z from 'zod';
export declare const SessionResponseSchema: z.ZodObject<{
    sessionId: z.ZodString;
}, z.core.$strip>;
export declare const SuccessResponseSchema: z.ZodObject<{
    success: z.ZodBoolean;
    reason: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
//# sourceMappingURL=responseSchemas.d.ts.map