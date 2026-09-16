import { jsx as _jsx } from "react/jsx-runtime";
import { Auth } from "./Auth.js";
import { State, } from "./types.js";
// PUBLIC API
export function LoginForm({ appearance, logo, socialLayout, }) {
    return (_jsx(Auth, { appearance: appearance, logo: logo, socialLayout: socialLayout, state: State.Login }));
}
// PUBLIC API
export function SignupForm({ appearance, logo, socialLayout, additionalFields, }) {
    return (_jsx(Auth, { appearance: appearance, logo: logo, socialLayout: socialLayout, state: State.Signup, additionalSignupFields: additionalFields }));
}
// PUBLIC API
export function ForgotPasswordForm({ appearance, logo, socialLayout, }) {
    return (_jsx(Auth, { appearance: appearance, logo: logo, socialLayout: socialLayout, state: State.ForgotPassword }));
}
// PUBLIC API
export function ResetPasswordForm({ appearance, logo, socialLayout, }) {
    return (_jsx(Auth, { appearance: appearance, logo: logo, socialLayout: socialLayout, state: State.ResetPassword }));
}
// PUBLIC API
export function VerifyEmailForm({ appearance, logo, socialLayout, }) {
    return (_jsx(Auth, { appearance: appearance, logo: logo, socialLayout: socialLayout, state: State.VerifyEmail }));
}
