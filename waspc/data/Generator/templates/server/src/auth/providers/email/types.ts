import { AdditionalSignupFieldsConfig as AdditionalSignupFieldsConfigGeneric } from '../types'

export type GetVerificationEmailContentFn = (params: { verificationLink: string }) => EmailContent;

export type GetPasswordResetEmailContentFn = (params: { passwordResetLink: string }) => EmailContent;

type EmailContent = {
    subject: string;
    html: string;
    text: string;
}

export const tokenVerificationErrors = {
    TokenExpiredError: 'TokenExpiredError',
};

export type AdditionalSignupFieldsConfig = AdditionalSignupFieldsConfigGeneric<"email" | "password">