import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { AuthContext } from "@wasp.sh/lib-auth/browser";
import { useMemo, useState } from "react";
import styles from "./Auth.module.css";
import "./internal/auth-styles.css";
import { LoginSignupForm } from "./internal/common/LoginSignupForm.js";
import { ForgotPasswordForm } from "./internal/email/ForgotPasswordForm.js";
import { ResetPasswordForm } from "./internal/email/ResetPasswordForm.js";
import { VerifyEmailForm } from "./internal/email/VerifyEmailForm.js";
import { MessageError, MessageSuccess } from "./internal/Message.js";
import { tokenObjToCSSVars } from "./internal/util.js";
import { State, } from "./types.js";
const logoStyle = { height: "3rem" };
const titles = {
    login: "Log in to your account",
    signup: "Create a new account",
    "forgot-password": "Forgot your password?",
    "reset-password": "Reset your password",
    "verify-email": "Email verification",
};
export function Auth({ state, appearance, logo, socialLayout = "horizontal", additionalSignupFields, }) {
    const [errorMessage, setErrorMessage] = useState(null);
    const [successMessage, setSuccessMessage] = useState(null);
    const [isLoading, setIsLoading] = useState(false);
    const customStyle = useMemo(() => ({
        ...tokenObjToCSSVars("color", appearance?.colors ?? {}),
        ...tokenObjToCSSVars("font-size", appearance?.fontSizes ?? {}),
    }), [appearance]);
    const socialButtonsDirection = socialLayout === "vertical" ? "vertical" : "horizontal";
    return (_jsxs("div", { className: styles.container, style: customStyle, children: [_jsxs("div", { children: [logo && _jsx("img", { style: logoStyle, src: logo, alt: "Your Company" }), _jsx("h2", { className: styles.headerText, children: titles[state] })] }), errorMessage && (_jsxs(MessageError, { children: [errorMessage.title, errorMessage.description && ": ", errorMessage.description] })), successMessage && _jsx(MessageSuccess, { children: successMessage }), _jsxs(AuthContext.Provider, { value: { isLoading, setIsLoading, setErrorMessage, setSuccessMessage }, children: [(state === State.Login || state === State.Signup) && (_jsx(LoginSignupForm, { state: state, socialButtonsDirection: socialButtonsDirection, additionalSignupFields: additionalSignupFields })), state === State.ForgotPassword && _jsx(ForgotPasswordForm, {}), state === State.ResetPassword && _jsx(ResetPasswordForm, {}), state === State.VerifyEmail && _jsx(VerifyEmailForm, {})] })] }));
}
