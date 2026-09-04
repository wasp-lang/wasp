import type { EmailContent, WaspAuthRuntime } from "../types.js";
/**
 * The email method's link and mail helpers. Bound to the runtime the adapter
 * was created with (see `bindEmailHelpers`), so the SDK can re-export them
 * as the `wasp/server/auth/email` public API.
 */
export type EmailHelpers = ReturnType<typeof makeEmailHelpers>;
export declare function makeEmailHelpers(runtime: WaspAuthRuntime): {
    createEmailVerificationLink: (email: string, clientRoute: string) => Promise<string>;
    createPasswordResetLink: (email: string, clientRoute: string) => Promise<string>;
    sendPasswordResetEmail: (email: string, content: {
        from?: {
            name?: string;
            email: string;
        };
        to: string;
    } & EmailContent) => Promise<void>;
    sendEmailVerificationEmail: (email: string, content: {
        from?: {
            name?: string;
            email: string;
        };
        to: string;
    } & EmailContent) => Promise<void>;
};
export declare function isEmailResendAllowed<Field extends "emailVerificationSentAt" | "passwordResetSentAt">(fields: {
    [field in Field]?: unknown;
}, field: Field, resendInterval?: number): {
    isResendAllowed: boolean;
    timeLeft: number;
};
