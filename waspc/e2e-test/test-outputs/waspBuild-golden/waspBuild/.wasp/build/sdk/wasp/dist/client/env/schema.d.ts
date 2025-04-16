import * as z from 'zod';
export declare const clientEnvSchema: z.ZodObject<z.objectUtil.extendShape<{}, {
    REACT_APP_API_URL: z.ZodDefault<z.ZodString>;
}>, "strip", z.ZodTypeAny, {
    REACT_APP_API_URL: string;
}, {
    REACT_APP_API_URL?: string | undefined;
}>;
