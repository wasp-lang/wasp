import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useAuthContext } from "@wasp.sh/lib-auth/browser";
import { useForm } from "react-hook-form";
import { requestPasswordReset } from "../../../actions.js";
import { Form, FormError, FormInput, FormItemGroup, FormLabel, SubmitButton, } from "../Form.js";
export const ForgotPasswordForm = () => {
    const { register, handleSubmit, reset, formState: { errors }, } = useForm();
    const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } = useAuthContext();
    const onSubmit = async (data) => {
        setIsLoading(true);
        setErrorMessage(null);
        setSuccessMessage(null);
        try {
            await requestPasswordReset(data);
            reset();
            setSuccessMessage("Check your email for a password reset link.");
        }
        catch (error) {
            const e = error;
            setErrorMessage({ title: e.message, description: e.data?.data?.message });
        }
        finally {
            setIsLoading(false);
        }
    };
    return (_jsxs(Form, { onSubmit: handleSubmit(onSubmit), children: [_jsxs(FormItemGroup, { children: [_jsx(FormLabel, { children: "E-mail" }), _jsx(FormInput, { ...register("email", { required: "Email is required" }), type: "email", disabled: isLoading }), errors.email && _jsx(FormError, { children: errors.email.message })] }), _jsx(FormItemGroup, { children: _jsx(SubmitButton, { type: "submit", disabled: isLoading, children: "Send password reset email" }) })] }));
};
