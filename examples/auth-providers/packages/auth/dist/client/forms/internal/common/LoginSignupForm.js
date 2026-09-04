import { jsxs as _jsxs, jsx as _jsx, Fragment as _Fragment } from "react/jsx-runtime";
import { useAuthContext } from "@wasp.sh/lib-auth/browser";
import { useForm } from "react-hook-form";
import { useNavigate } from "react-router";
import { isMethodEnabled, signInUrl } from "../../../actions.js";
import { getClientOptions } from "../../../runtime.js";
import { Form, FormError, FormInput, FormItemGroup, FormLabel, FormTextarea, SubmitButton, } from "../Form.js";
import "../auth-styles.css";
import { useEmail } from "../email/useEmail.js";
import { SocialButton } from "../social/SocialButton.js";
import * as SocialIcons from "../social/SocialIcons.js";
import { useUsernameAndPassword } from "../usernameAndPassword/useUsernameAndPassword.js";
import { clsx } from "../util.js";
import styles from "./LoginSignupForm.module.css";
const socialIcons = {
    slack: SocialIcons.Slack,
    discord: SocialIcons.Discord,
    google: SocialIcons.Google,
    keycloak: SocialIcons.Keycloak,
    github: SocialIcons.GitHub,
    microsoft: SocialIcons.Microsoft,
};
const socialOrder = [
    "slack",
    "discord",
    "google",
    "keycloak",
    "github",
    "microsoft",
];
export const LoginSignupForm = ({ state, socialButtonsDirection = "horizontal", additionalSignupFields, }) => {
    const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } = useAuthContext();
    const options = getClientOptions();
    const isLogin = state === "login";
    const cta = isLogin ? "Log in" : "Sign up";
    const navigate = useNavigate();
    const isEmailEnabled = isMethodEnabled("email");
    const isUsernameEnabled = isMethodEnabled("usernameAndPassword");
    const enabledSocial = socialOrder.filter((name) => isMethodEnabled(name));
    const isSocialAuthEnabled = enabledSocial.length > 0;
    const isAnyPasswordBasedAuthEnabled = isEmailEnabled || isUsernameEnabled;
    const onErrorHandler = (error) => {
        setErrorMessage({
            title: error.message,
            description: error.data?.data?.message,
        });
    };
    const hookForm = useForm();
    const { register, formState: { errors }, handleSubmit: hookFormHandleSubmit, } = hookForm;
    const usernameAndPassword = useUsernameAndPassword({
        isLogin,
        onError: onErrorHandler,
        onSuccess() {
            navigate(options.onAuthSucceededRedirectTo);
        },
    });
    const email = useEmail({
        isLogin,
        onError: onErrorHandler,
        showEmailVerificationPending() {
            hookForm.reset();
            setSuccessMessage(`You've signed up successfully! Check your email for the confirmation link.`);
        },
        onLoginSuccess() {
            navigate(options.onAuthSucceededRedirectTo);
        },
    });
    const handleSubmit = isEmailEnabled
        ? email.handleSubmit
        : usernameAndPassword.handleSubmit;
    async function onSubmit(data) {
        setIsLoading(true);
        setErrorMessage(null);
        setSuccessMessage(null);
        try {
            await handleSubmit(data);
        }
        finally {
            setIsLoading(false);
        }
    }
    return (_jsxs(_Fragment, { children: [isSocialAuthEnabled && (_jsxs("div", { className: styles.socialAuth, children: [_jsxs("div", { className: styles.socialAuthLabel, children: [cta, " with"] }), _jsx("div", { className: clsx(styles.socialAuthButtons, styles[socialButtonsDirection]), children: enabledSocial.map((name) => {
                            const Icon = socialIcons[name];
                            return (_jsx(SocialButton, { href: signInUrl(name), children: _jsx(Icon, {}) }, name));
                        }) })] })), isSocialAuthEnabled && isAnyPasswordBasedAuthEnabled && (_jsxs("div", { className: styles.orContinueWith, children: [_jsx("div", { className: styles.orContinueWithLineContainer, children: _jsx("div", { className: styles.orContinueWithLine }) }), _jsx("div", { className: styles.orContinueWithTextContainer, children: _jsx("span", { className: styles.orContinueWithText, children: "Or continue with" }) })] })), isAnyPasswordBasedAuthEnabled && (_jsxs(Form, { onSubmit: hookFormHandleSubmit(onSubmit), children: [isUsernameEnabled && (_jsxs(FormItemGroup, { children: [_jsx(FormLabel, { children: "Username" }), _jsx(FormInput, { ...register("username", { required: "Username is required" }), type: "text", disabled: isLoading }), errors.username && (_jsx(FormError, { children: errors.username.message }))] })), isEmailEnabled && (_jsxs(FormItemGroup, { children: [_jsx(FormLabel, { children: "E-mail" }), _jsx(FormInput, { ...register("email", { required: "Email is required" }), type: "email", disabled: isLoading }), errors.email && _jsx(FormError, { children: errors.email.message })] })), _jsxs(FormItemGroup, { children: [_jsx(FormLabel, { children: "Password" }), _jsx(FormInput, { ...register("password", { required: "Password is required" }), type: "password", disabled: isLoading }), errors.password && (_jsx(FormError, { children: errors.password.message }))] }), _jsx(AdditionalFormFields, { hookForm: hookForm, formState: { isLoading }, additionalSignupFields: additionalSignupFields }), _jsx(FormItemGroup, { children: _jsx(SubmitButton, { type: "submit", disabled: isLoading, children: cta }) })] }))] }));
};
function AdditionalFormFields({ hookForm, formState: { isLoading }, additionalSignupFields, }) {
    const { register, formState: { errors }, } = hookForm;
    function renderField(field, Component, props) {
        const errorMessage = errors[field.name]?.message;
        return (_jsxs(FormItemGroup, { children: [_jsx(FormLabel, { children: field.label }), _jsx(Component, { ...register(field.name, field.validations), ...props, disabled: isLoading }), errorMessage && _jsx(FormError, { children: errorMessage })] }, field.name));
    }
    if (typeof additionalSignupFields === "function") {
        return additionalSignupFields(hookForm, {
            isLoading,
        });
    }
    return (additionalSignupFields &&
        additionalSignupFields.map((field) => {
            if (typeof field === "function") {
                return field(hookForm, { isLoading });
            }
            switch (field.type) {
                case "input":
                    return renderField(field, FormInput, { type: "text" });
                case "textarea":
                    return renderField(field, FormTextarea);
                default:
                    throw new Error(`Unsupported additional signup field type: ${field.type}`);
            }
        }));
}
