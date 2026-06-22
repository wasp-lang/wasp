import type { ChangeEvent, FormEvent } from "react";
import type { ErrorMessage } from "../messages";

export type AuthFormFields = Record<string, string>;

export type AuthFormStatus = "idle" | "submitting" | "success" | "error";

export type AuthFieldErrors<Fields extends AuthFormFields> = Partial<
  Record<keyof Fields & string, string>
>;

export type AuthFormValidationResult<Fields extends AuthFormFields> =
  | {
      fieldErrors?: AuthFieldErrors<Fields>;
      errorMessage?: ErrorMessage;
    }
  | null
  | undefined
  | void;

export type AuthFormValidator<Fields extends AuthFormFields> = (
  fields: Fields,
) => AuthFormValidationResult<Fields>;

export type AuthFormSubmitResult = {
  successMessage?: string | null;
} | void;

export type AuthFormSubmit<
  Fields extends AuthFormFields,
  Result = AuthFormSubmitResult,
> = (fields: Fields) => Promise<Result> | Result;

export type AuthFormSuccessMessage<Fields extends AuthFormFields, Result> =
  | string
  | null
  | ((args: { fields: Fields; result: Result }) => string | null | undefined);

export type AuthFormFieldChangeEvent = ChangeEvent<
  HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement
>;

export type AuthFormFieldProps = {
  name: string;
  value: string;
  disabled: boolean;
  "aria-invalid"?: true;
  onChange: (eventOrValue: AuthFormFieldChangeEvent | string) => void;
};

export type AuthFormSubmitEvent = FormEvent<HTMLFormElement>;

export type AuthFormSubmitOutcome<Result> =
  | { ok: true; result: Result; successMessage: string | null }
  | { ok: false; error: unknown; errorMessage: ErrorMessage };
