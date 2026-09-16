import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useAuthContext } from "@wasp.sh/lib-auth/browser";
import { useForm } from "react-hook-form";
import { useLocation } from "react-router";
import { resetPassword } from "../../../actions.js";
import { Form, FormError, FormInput, FormItemGroup, FormLabel, SubmitButton, } from "../Form.js";
export const ResetPasswordForm = () => {
    const { register, handleSubmit, reset, formState: { errors }, } = useForm();
    const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } = useAuthContext();
    const location = useLocation();
    const token = new URLSearchParams(location.search).get("token");
    const onSubmit = async (data) => {
        if (!token) {
            setErrorMessage({
                title: "The token is missing from the URL. Please check the link you received in your email.",
            });
            return;
        }
        if (!data.password || data.password !== data.passwordConfirmation) {
            setErrorMessage({ title: `Passwords don't match!` });
            return;
        }
        setIsLoading(true);
        setErrorMessage(null);
        setSuccessMessage(null);
        try {
            await resetPassword({ password: data.password, token });
            reset();
            setSuccessMessage("Your password has been reset.");
        }
        catch (error) {
            const e = error;
            setErrorMessage({ title: e.message, description: e.data?.data?.message });
        }
        finally {
            setIsLoading(false);
        }
    };
    return (_jsxs(Form, { onSubmit: handleSubmit(onSubmit), children: [_jsxs(FormItemGroup, { children: [_jsx(FormLabel, { children: "New password" }), _jsx(FormInput, { ...register("password", { required: "Password is required" }), type: "password", disabled: isLoading }), errors.password && _jsx(FormError, { children: errors.password.message })] }), _jsxs(FormItemGroup, { children: [_jsx(FormLabel, { children: "Confirm new password" }), _jsx(FormInput, { ...register("passwordConfirmation", {
                            required: "Password confirmation is required",
                        }), type: "password", disabled: isLoading }), errors.passwordConfirmation && (_jsx(FormError, { children: errors.passwordConfirmation.message }))] }), _jsx(FormItemGroup, { children: _jsx(SubmitButton, { type: "submit", disabled: isLoading, children: "Reset password" }) })] }));
};
