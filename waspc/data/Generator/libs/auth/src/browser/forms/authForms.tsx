import { Auth } from "./Auth.js";
import type { AuthFormsConfig } from "./config.js";
import { State } from "./state.js";
import type { AdditionalSignupFields, CustomizationOptions } from "./types.js";

export type AuthForms = {
  LoginForm: (props: CustomizationOptions) => React.JSX.Element;
  SignupForm: (
    props: CustomizationOptions & { additionalFields?: AdditionalSignupFields },
  ) => React.JSX.Element;
  ForgotPasswordForm: (props: CustomizationOptions) => React.JSX.Element;
  ResetPasswordForm: (props: CustomizationOptions) => React.JSX.Element;
  VerifyEmailForm: (props: CustomizationOptions) => React.JSX.Element;
};

/** Creates the app's auth form components, bound to the app's auth config. */
export function createAuthForms(config: AuthFormsConfig): AuthForms {
  function LoginForm({
    appearance,
    logo,
    socialLayout,
  }: CustomizationOptions): React.JSX.Element {
    return (
      <Auth
        config={config}
        appearance={appearance}
        logo={logo}
        socialLayout={socialLayout}
        state={State.Login}
      />
    );
  }

  function SignupForm({
    appearance,
    logo,
    socialLayout,
    additionalFields,
  }: CustomizationOptions & {
    additionalFields?: AdditionalSignupFields;
  }): React.JSX.Element {
    return (
      <Auth
        config={config}
        appearance={appearance}
        logo={logo}
        socialLayout={socialLayout}
        state={State.Signup}
        additionalSignupFields={additionalFields}
      />
    );
  }

  function ForgotPasswordForm({
    appearance,
    logo,
    socialLayout,
  }: CustomizationOptions): React.JSX.Element {
    return (
      <Auth
        config={config}
        appearance={appearance}
        logo={logo}
        socialLayout={socialLayout}
        state={State.ForgotPassword}
      />
    );
  }

  function ResetPasswordForm({
    appearance,
    logo,
    socialLayout,
  }: CustomizationOptions): React.JSX.Element {
    return (
      <Auth
        config={config}
        appearance={appearance}
        logo={logo}
        socialLayout={socialLayout}
        state={State.ResetPassword}
      />
    );
  }

  function VerifyEmailForm({
    appearance,
    logo,
    socialLayout,
  }: CustomizationOptions): React.JSX.Element {
    return (
      <Auth
        config={config}
        appearance={appearance}
        logo={logo}
        socialLayout={socialLayout}
        state={State.VerifyEmail}
      />
    );
  }

  return {
    LoginForm,
    SignupForm,
    ForgotPasswordForm,
    ResetPasswordForm,
    VerifyEmailForm,
  };
}
