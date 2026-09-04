import type { RegisterOptions, UseFormReturn } from "react-hook-form";

export type LoginSignupFormFields = { [key: string]: string };

export enum State {
  Login = "login",
  Signup = "signup",
  ForgotPassword = "forgot-password",
  ResetPassword = "reset-password",
  VerifyEmail = "verify-email",
}

// PUBLIC API
export type CustomizationOptions = {
  logo?: string;
  socialLayout?: "horizontal" | "vertical";
  appearance?: {
    // ATTENTION: Keep this list in sync with the one at `./internal/auth-styles.css`
    colors?: {
      waspYellow?: string;
      gray700?: string;
      gray600?: string;
      gray500?: string;
      gray400?: string;
      red?: string;
      darkRed?: string;
      green?: string;
      brand?: string;
      brandAccent?: string;
      errorBackground?: string;
      errorText?: string;
      successBackground?: string;
      successText?: string;
      submitButtonText?: string;
      formErrorText?: string;
    };
    fontSizes?: { sm?: string };
  };
};

export type FormState = { isLoading: boolean };

export type AdditionalSignupFieldRenderFn = (
  hookForm: UseFormReturn<LoginSignupFormFields>,
  formState: FormState,
) => React.ReactNode;

export type AdditionalSignupField = {
  name: string;
  label: string;
  type: "input" | "textarea";
  validations?: RegisterOptions<LoginSignupFormFields>;
};

export type AdditionalSignupFields =
  | (AdditionalSignupField | AdditionalSignupFieldRenderFn)[]
  | AdditionalSignupFieldRenderFn;
