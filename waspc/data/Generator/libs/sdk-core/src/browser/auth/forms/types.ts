import type { RegisterOptions, UseFormReturn } from "react-hook-form";
import type { LoginSignupFormFields } from "./LoginSignupForm.js";
export type CustomizationOptions = {
  logo?: string;
  socialLayout?: "horizontal" | "vertical";
  appearance?: {
    // ATTENTION: Keep this list in sync with the one at `./auth-styles.css`
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
    fontSizes?: {
      sm?: string;
    };
  };
};

// PRIVATE API
export type FormState = {
  isLoading: boolean;
};

// PRIVATE API
export type AdditionalSignupFieldRenderFn = (
  hookForm: UseFormReturn<LoginSignupFormFields>,
  formState: FormState,
) => React.ReactNode;

// PRIVATE API
export type AdditionalSignupField = {
  name: string;
  label: string;
  type: "input" | "textarea";
  validations?: RegisterOptions<LoginSignupFormFields>;
};

// PRIVATE API
export type AdditionalSignupFields =
  | (AdditionalSignupField | AdditionalSignupFieldRenderFn)[]
  | AdditionalSignupFieldRenderFn;
