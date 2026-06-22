import { useState } from "react";
import { getDefaultErrorMessage, type ErrorMessage } from "../messages";
import type {
  AuthFieldErrors,
  AuthFormFieldChangeEvent,
  AuthFormFieldProps,
  AuthFormFields,
  AuthFormStatus,
  AuthFormSubmit,
  AuthFormSubmitEvent,
  AuthFormSubmitOutcome,
  AuthFormSubmitResult,
  AuthFormSuccessMessage,
  AuthFormValidationResult,
  AuthFormValidator,
} from "./types";

export type UseAuthFormOptions<
  Fields extends AuthFormFields,
  Result = AuthFormSubmitResult,
> = {
  initialFields: Fields;
  submit: AuthFormSubmit<Fields, Result>;
  validate?: AuthFormValidator<Fields>;
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

export function useAuthForm<
  Fields extends AuthFormFields,
  Result = AuthFormSubmitResult,
>({
  initialFields,
  submit,
  validate,
  successMessage,
  getErrorMessage = getDefaultErrorMessage,
  onSuccess,
  onError,
  resetOnSuccess = false,
}: UseAuthFormOptions<Fields, Result>) {
  const [fields, setFields] = useState<Fields>(() =>
    cloneFields(initialFields),
  );
  const [fieldErrors, setFieldErrors] = useState<AuthFieldErrors<Fields>>({});
  const [errorMessage, setErrorMessage] = useState<ErrorMessage | null>(null);
  const [successMessageState, setSuccessMessageState] = useState<string | null>(
    null,
  );
  const [status, setStatus] = useState<AuthFormStatus>("idle");
  const isSubmitting = status === "submitting";

  function setField<Name extends keyof Fields & string>(
    name: Name,
    value: Fields[Name],
  ) {
    setFields((currentFields) => ({ ...currentFields, [name]: value }));
    setFieldErrors((currentErrors) => {
      if (!currentErrors[name]) {
        return currentErrors;
      }

      const nextErrors = { ...currentErrors };
      delete nextErrors[name];
      return nextErrors;
    });
  }

  function getFieldProps<Name extends keyof Fields & string>(
    name: Name,
  ): AuthFormFieldProps {
    return {
      name,
      value: fields[name] ?? "",
      disabled: isSubmitting,
      ...(fieldErrors[name] ? { "aria-invalid": true as const } : {}),
      onChange(eventOrValue) {
        setField(name, getChangeValue(eventOrValue) as Fields[Name]);
      },
    };
  }

  function setFormError(errorMessage: ErrorMessage | null) {
    setErrorMessage(errorMessage);
    if (errorMessage !== null) {
      setStatus("error");
    }
  }

  function clearMessages() {
    setErrorMessage(null);
    setSuccessMessageState(null);
  }

  function reset(nextFields: Fields = initialFields) {
    setFields(cloneFields(nextFields));
    setFieldErrors({});
    setErrorMessage(null);
    setSuccessMessageState(null);
    setStatus("idle");
  }

  async function submitForm(
    event?: AuthFormSubmitEvent,
  ): Promise<AuthFormSubmitOutcome<Result>> {
    event?.preventDefault();
    clearMessages();

    const validationResult = validate?.(fields);
    if (hasValidationErrors(validationResult)) {
      const nextFieldErrors = validationResult.fieldErrors ?? {};
      const nextErrorMessage = validationResult.errorMessage ?? {
        title: "Please check the form fields.",
      };
      setFieldErrors(nextFieldErrors);
      setErrorMessage(nextErrorMessage);
      setStatus("error");
      return {
        ok: false,
        error: validationResult,
        errorMessage: nextErrorMessage,
      };
    }

    setFieldErrors({});
    setStatus("submitting");

    try {
      const result = await submit(fields);
      const nextSuccessMessage = resolveSuccessMessage({
        fields,
        result,
        successMessage,
      });
      setSuccessMessageState(nextSuccessMessage);
      setStatus("success");
      if (resetOnSuccess) {
        setFields(cloneFields(initialFields));
      }
      await onSuccess?.({ fields, result, successMessage: nextSuccessMessage });
      return { ok: true, result, successMessage: nextSuccessMessage };
    } catch (error: unknown) {
      const nextErrorMessage = getErrorMessage(error);
      setErrorMessage(nextErrorMessage);
      setStatus("error");
      await onError?.({ fields, error, errorMessage: nextErrorMessage });
      return { ok: false, error, errorMessage: nextErrorMessage };
    }
  }

  return {
    fields,
    fieldErrors,
    errorMessage,
    successMessage: successMessageState,
    status,
    isIdle: status === "idle",
    isSubmitting,
    isSuccess: status === "success",
    isError: status === "error",
    setField,
    setFieldErrors,
    setErrorMessage: setFormError,
    setSuccessMessage: setSuccessMessageState,
    getFieldProps,
    clearMessages,
    reset,
    submit: submitForm,
  };
}

export function validateRequiredFields<Fields extends AuthFormFields>(
  fields: Fields,
  fieldNames: Array<keyof Fields & string>,
  labels: Partial<Record<keyof Fields & string, string>> = {},
): AuthFormValidationResult<Fields> {
  const fieldErrors: AuthFieldErrors<Fields> = {};

  for (const fieldName of fieldNames) {
    if (!fields[fieldName]?.trim()) {
      fieldErrors[fieldName] = `${labels[fieldName] ?? fieldName} is required`;
    }
  }

  return Object.keys(fieldErrors).length > 0 ? { fieldErrors } : null;
}

export function mergeValidationResults<Fields extends AuthFormFields>(
  ...results: Array<AuthFormValidationResult<Fields>>
): AuthFormValidationResult<Fields> {
  const fieldErrors: AuthFieldErrors<Fields> = {};
  let errorMessage: ErrorMessage | undefined;

  for (const result of results) {
    if (!hasValidationErrors(result)) {
      continue;
    }

    Object.assign(fieldErrors, result.fieldErrors);
    errorMessage ??= result.errorMessage;
  }

  return Object.keys(fieldErrors).length > 0 || errorMessage
    ? { fieldErrors, errorMessage }
    : null;
}

function hasValidationErrors<Fields extends AuthFormFields>(
  result: AuthFormValidationResult<Fields>,
): result is Exclude<
  AuthFormValidationResult<Fields>,
  null | undefined | void
> {
  return Boolean(
    result &&
      ((result.fieldErrors && Object.keys(result.fieldErrors).length > 0) ||
        result.errorMessage),
  );
}

function resolveSuccessMessage<Fields extends AuthFormFields, Result>({
  fields,
  result,
  successMessage,
}: {
  fields: Fields;
  result: Result;
  successMessage?: AuthFormSuccessMessage<Fields, Result>;
}): string | null {
  if (typeof successMessage === "function") {
    return successMessage({ fields, result }) ?? null;
  }

  if (successMessage !== undefined) {
    return successMessage;
  }

  if (isResultWithSuccessMessage(result)) {
    return result.successMessage ?? null;
  }

  return null;
}

function isResultWithSuccessMessage(
  result: unknown,
): result is { successMessage?: string | null } {
  return (
    typeof result === "object" && result !== null && "successMessage" in result
  );
}

function getChangeValue(
  eventOrValue: AuthFormFieldChangeEvent | string,
): string {
  return typeof eventOrValue === "string"
    ? eventOrValue
    : eventOrValue.target.value;
}

function cloneFields<Fields extends AuthFormFields>(fields: Fields): Fields {
  return { ...fields };
}
