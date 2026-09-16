import { Auth } from "./Auth.js";
import {
  State,
  type AdditionalSignupFields,
  type CustomizationOptions,
} from "./types.js";

// PUBLIC API
export function LoginForm({
  appearance,
  logo,
  socialLayout,
}: CustomizationOptions): React.JSX.Element {
  return (
    <Auth
      appearance={appearance}
      logo={logo}
      socialLayout={socialLayout}
      state={State.Login}
    />
  );
}

// PUBLIC API
export function SignupForm({
  appearance,
  logo,
  socialLayout,
  additionalFields,
}: CustomizationOptions & {
  additionalFields?: AdditionalSignupFields;
}): React.JSX.Element {
  return (
    <Auth
      appearance={appearance}
      logo={logo}
      socialLayout={socialLayout}
      state={State.Signup}
      additionalSignupFields={additionalFields}
    />
  );
}

// PUBLIC API
export function ForgotPasswordForm({
  appearance,
  logo,
  socialLayout,
}: CustomizationOptions): React.JSX.Element {
  return (
    <Auth
      appearance={appearance}
      logo={logo}
      socialLayout={socialLayout}
      state={State.ForgotPassword}
    />
  );
}

// PUBLIC API
export function ResetPasswordForm({
  appearance,
  logo,
  socialLayout,
}: CustomizationOptions): React.JSX.Element {
  return (
    <Auth
      appearance={appearance}
      logo={logo}
      socialLayout={socialLayout}
      state={State.ResetPassword}
    />
  );
}

// PUBLIC API
export function VerifyEmailForm({
  appearance,
  logo,
  socialLayout,
}: CustomizationOptions): React.JSX.Element {
  return (
    <Auth
      appearance={appearance}
      logo={logo}
      socialLayout={socialLayout}
      state={State.VerifyEmail}
    />
  );
}
