import { useEffect, useRef } from "react";
import type { ErrorMessage } from "../messages";
import type {
  AuthFormFields,
  AuthFormSubmit,
  AuthFormSubmitResult,
  AuthFormSuccessMessage,
  AuthFormValidator,
} from "./types";
import {
  mergeValidationResults,
  useAuthForm,
  validateRequiredFields,
} from "./useAuthForm";

export type AuthIdentityFieldName = "email" | "username";

export type PasswordAuthLabels<Fields extends AuthFormFields> = Partial<
  Record<keyof Fields & string, string>
>;

export type PasswordAuthFormOptions<
  Fields extends AuthFormFields,
  Result = AuthFormSubmitResult,
> = {
  identityField: AuthIdentityFieldName;
  initialFields?: Partial<Fields>;
  submit: AuthFormSubmit<Fields, Result>;
  validate?: AuthFormValidator<Fields>;
  labels?: PasswordAuthLabels<Fields>;
  successMessage?: AuthFormSuccessMessage<Fields, Result>;
  getErrorMessage?: (error: unknown) => ErrorMessage;
  onSuccess?: (args: {
    fields: Fields;
    result: Result;
    successMessage: string | null;
  }) => Promise<void> | void;
  onError?: (args: {
    fields: Fields;
    error: unknown;
    errorMessage: ErrorMessage;
  }) => Promise<void> | void;
  resetOnSuccess?: boolean;
};

export function useLoginForm<
  Fields extends AuthFormFields,
  Result = AuthFormSubmitResult,
>(options: PasswordAuthFormOptions<Fields, Result>) {
  return usePasswordAuthForm(options);
}

export function useSignupForm<
  Fields extends AuthFormFields,
  Result = AuthFormSubmitResult,
>(options: PasswordAuthFormOptions<Fields, Result>) {
  return usePasswordAuthForm(options);
}

export type ForgotPasswordFormFields = {
  email: string;
};

export type ForgotPasswordFormOptions<Result = AuthFormSubmitResult> = {
  initialEmail?: string;
  submit: AuthFormSubmit<ForgotPasswordFormFields, Result>;
  successMessage?: AuthFormSuccessMessage<ForgotPasswordFormFields, Result>;
  getErrorMessage?: (error: unknown) => ErrorMessage;
  onSuccess?: (args: {
    fields: ForgotPasswordFormFields;
    result: Result;
    successMessage: string | null;
  }) => Promise<void> | void;
  onError?: (args: {
    fields: ForgotPasswordFormFields;
    error: unknown;
    errorMessage: ErrorMessage;
  }) => Promise<void> | void;
  resetOnSuccess?: boolean;
};

export function useForgotPasswordForm<Result = AuthFormSubmitResult>({
  initialEmail = "",
  submit,
  successMessage = "Check your email for a password reset link.",
  getErrorMessage,
  onSuccess,
  onError,
  resetOnSuccess = true,
}: ForgotPasswordFormOptions<Result>) {
  return useAuthForm<ForgotPasswordFormFields, Result>({
    initialFields: { email: initialEmail },
    submit,
    successMessage,
    getErrorMessage,
    onSuccess,
    onError,
    resetOnSuccess,
    validate(fields) {
      return validateRequiredFields(fields, ["email"], { email: "Email" });
    },
  });
}

export type ResetPasswordFormFields = {
  password: string;
  passwordConfirmation: string;
};

export type ResetPasswordSubmitFields = ResetPasswordFormFields & {
  token: string;
};

export type ResetPasswordFormOptions<Result = AuthFormSubmitResult> = {
  token?: string | null;
  submit: AuthFormSubmit<ResetPasswordSubmitFields, Result>;
  successMessage?: AuthFormSuccessMessage<ResetPasswordFormFields, Result>;
  getErrorMessage?: (error: unknown) => ErrorMessage;
  missingTokenMessage?: ErrorMessage;
  onSuccess?: (args: {
    fields: ResetPasswordFormFields;
    result: Result;
    successMessage: string | null;
  }) => Promise<void> | void;
  onError?: (args: {
    fields: ResetPasswordFormFields;
    error: unknown;
    errorMessage: ErrorMessage;
  }) => Promise<void> | void;
  resetOnSuccess?: boolean;
};

export function useResetPasswordForm<Result = AuthFormSubmitResult>({
  token,
  submit,
  successMessage = "Your password has been reset.",
  getErrorMessage,
  missingTokenMessage = {
    title:
      "The token is missing from the URL. Please check the link you received in your email.",
  },
  onSuccess,
  onError,
  resetOnSuccess = true,
}: ResetPasswordFormOptions<Result>) {
  return useAuthForm<ResetPasswordFormFields, Result>({
    initialFields: { password: "", passwordConfirmation: "" },
    submit(fields) {
      return submit({ ...fields, token: token ?? "" });
    },
    successMessage,
    getErrorMessage,
    onSuccess,
    onError,
    resetOnSuccess,
    validate(fields) {
      return mergeValidationResults(
        token ? null : { errorMessage: missingTokenMessage },
        validateRequiredFields(fields, ["password", "passwordConfirmation"], {
          password: "Password",
          passwordConfirmation: "Password confirmation",
        }),
        fields.password !== fields.passwordConfirmation
          ? {
              fieldErrors: {
                passwordConfirmation: "Passwords don't match",
              },
            }
          : null,
      );
    },
  });
}

export type VerifyEmailFormOptions<Result = AuthFormSubmitResult> = {
  token?: string | null;
  verify: AuthFormSubmit<{ token: string }, Result>;
  autoSubmit?: boolean;
  successMessage?: AuthFormSuccessMessage<{ token: string }, Result>;
  getErrorMessage?: (error: unknown) => ErrorMessage;
  missingTokenMessage?: ErrorMessage;
  onSuccess?: (args: {
    fields: { token: string };
    result: Result;
    successMessage: string | null;
  }) => Promise<void> | void;
  onError?: (args: {
    fields: { token: string };
    error: unknown;
    errorMessage: ErrorMessage;
  }) => Promise<void> | void;
};

export function useVerifyEmail<Result = AuthFormSubmitResult>({
  token,
  verify,
  autoSubmit = true,
  successMessage = "Your email has been verified. You can now log in.",
  getErrorMessage,
  missingTokenMessage = {
    title:
      "The token is missing from the URL. Please check the link you received in your email.",
  },
  onSuccess,
  onError,
}: VerifyEmailFormOptions<Result>) {
  const submittedTokenRef = useRef<{ token?: string | null } | null>(null);
  const form = useAuthForm<{ token: string }, Result>({
    initialFields: { token: token ?? "" },
    submit(fields) {
      return verify({ token: token ?? fields.token });
    },
    successMessage,
    getErrorMessage,
    onSuccess({ fields, result, successMessage }) {
      return onSuccess?.({
        fields: { token: token ?? fields.token },
        result,
        successMessage,
      });
    },
    onError({ fields, error, errorMessage }) {
      return onError?.({
        fields: { token: token ?? fields.token },
        error,
        errorMessage,
      });
    },
    validate(fields) {
      return (token ?? fields.token)
        ? null
        : { errorMessage: missingTokenMessage };
    },
  });

  useEffect(() => {
    if (
      !autoSubmit ||
      (submittedTokenRef.current !== null &&
        submittedTokenRef.current.token === token)
    ) {
      return;
    }

    submittedTokenRef.current = { token };
    void form.submit();
  }, [autoSubmit, form.submit, token]);

  return {
    ...form,
    verify: form.submit,
  };
}

function usePasswordAuthForm<
  Fields extends AuthFormFields,
  Result = AuthFormSubmitResult,
>({
  identityField,
  initialFields,
  submit,
  validate,
  labels,
  successMessage,
  getErrorMessage,
  onSuccess,
  onError,
  resetOnSuccess,
}: PasswordAuthFormOptions<Fields, Result>) {
  const authFields = {
    [identityField]: "",
    password: "",
    ...initialFields,
  } as unknown as Fields;

  return useAuthForm<Fields, Result>({
    initialFields: authFields,
    submit,
    successMessage,
    getErrorMessage,
    onSuccess,
    onError,
    resetOnSuccess,
    validate(fields) {
      return mergeValidationResults(
        validateRequiredFields(
          fields,
          [identityField, "password"] as Array<keyof Fields & string>,
          {
            [identityField]: identityField === "email" ? "Email" : "Username",
            password: "Password",
            ...labels,
          } as PasswordAuthLabels<Fields>,
        ),
        validate?.(fields),
      );
    },
  });
}
