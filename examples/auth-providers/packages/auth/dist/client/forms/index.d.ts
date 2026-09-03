import { type AdditionalSignupFields, type CustomizationOptions } from "./types.js";
export declare function LoginForm({ appearance, logo, socialLayout, }: CustomizationOptions): React.JSX.Element;
export declare function SignupForm({ appearance, logo, socialLayout, additionalFields, }: CustomizationOptions & {
    additionalFields?: AdditionalSignupFields;
}): React.JSX.Element;
export declare function ForgotPasswordForm({ appearance, logo, socialLayout, }: CustomizationOptions): React.JSX.Element;
export declare function ResetPasswordForm({ appearance, logo, socialLayout, }: CustomizationOptions): React.JSX.Element;
export declare function VerifyEmailForm({ appearance, logo, socialLayout, }: CustomizationOptions): React.JSX.Element;
