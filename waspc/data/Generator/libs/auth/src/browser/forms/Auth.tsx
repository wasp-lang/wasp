import { useMemo, useState } from "react";
import { AuthContext, type ErrorMessage } from "../context.js";
import styles from "./Auth.module.css";
import type { AuthFormsConfig } from "./config.js";
import { LoginSignupForm } from "./internal/common/LoginSignupForm.js";
import { DummyEmailSenderNotice } from "./internal/email/DummyEmailSenderNotice.js";
import { ForgotPasswordForm } from "./internal/email/ForgotPasswordForm.js";
import { ResetPasswordForm } from "./internal/email/ResetPasswordForm.js";
import { VerifyEmailForm } from "./internal/email/VerifyEmailForm.js";
import { MessageError, MessageSuccess } from "./Message.js";
import { State } from "./state.js";
import type { AdditionalSignupFields, CustomizationOptions } from "./types.js";
import { tokenObjToCSSVars } from "./util.js";

const logoStyle = {
  height: "3rem",
};

const titles: Record<State, string> = {
  login: "Log in to your account",
  signup: "Create a new account",
  "forgot-password": "Forgot your password?",
  "reset-password": "Reset your password",
  "verify-email": "Email verification",
};

export function Auth({
  config,
  state,
  appearance,
  logo,
  socialLayout = "horizontal",
  additionalSignupFields,
}: {
  config: AuthFormsConfig;
  state: State;
} & CustomizationOptions & {
    additionalSignupFields?: AdditionalSignupFields;
  }) {
  const [errorMessage, setErrorMessage] = useState<ErrorMessage | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  const [isLoading, setIsLoading] = useState(false);

  const customStyle = useMemo(
    () => ({
      ...tokenObjToCSSVars("color", appearance?.colors ?? {}),
      ...tokenObjToCSSVars("font-size", appearance?.fontSizes ?? {}),
    }),
    [appearance],
  );

  const title = titles[state];

  const socialButtonsDirection =
    socialLayout === "vertical" ? "vertical" : "horizontal";

  const emailActions = config.email;

  return (
    <div className={styles.container} style={customStyle}>
      <div>
        {logo && <img style={logoStyle} src={logo} alt="Your Company" />}
        <h2 className={styles.headerText}>{title}</h2>
      </div>

      {config.isDummyEmailSenderNoticeEnabled &&
        (state === "signup" || state === "forgot-password") && (
          <DummyEmailSenderNotice />
        )}
      {errorMessage && (
        <MessageError>
          {errorMessage.title}
          {errorMessage.description && ": "}
          {errorMessage.description}
        </MessageError>
      )}
      {successMessage && <MessageSuccess>{successMessage}</MessageSuccess>}
      <AuthContext.Provider
        value={{ isLoading, setIsLoading, setErrorMessage, setSuccessMessage }}
      >
        {(state === "login" || state === "signup") && (
          <LoginSignupForm
            config={config}
            state={state}
            socialButtonsDirection={socialButtonsDirection}
            additionalSignupFields={additionalSignupFields}
          />
        )}
        {emailActions && (
          <>
            {state === "forgot-password" && (
              <ForgotPasswordForm
                requestPasswordReset={emailActions.requestPasswordReset}
              />
            )}
            {state === "reset-password" && (
              <ResetPasswordForm resetPassword={emailActions.resetPassword} />
            )}
            {state === "verify-email" && (
              <VerifyEmailForm verifyEmail={emailActions.verifyEmail} />
            )}
          </>
        )}
      </AuthContext.Provider>
    </div>
  );
}
