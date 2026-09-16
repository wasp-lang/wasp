import { AuthContext, type ErrorMessage } from "@wasp.sh/lib-auth/browser";
import { useMemo, useState } from "react";

import styles from "./Auth.module.css";
import "./internal/auth-styles.css";
import { LoginSignupForm } from "./internal/common/LoginSignupForm.js";
import { ForgotPasswordForm } from "./internal/email/ForgotPasswordForm.js";
import { ResetPasswordForm } from "./internal/email/ResetPasswordForm.js";
import { VerifyEmailForm } from "./internal/email/VerifyEmailForm.js";
import { MessageError, MessageSuccess } from "./internal/Message.js";
import { tokenObjToCSSVars } from "./internal/util.js";
import {
  State,
  type AdditionalSignupFields,
  type CustomizationOptions,
} from "./types.js";

const logoStyle = { height: "3rem" };

const titles: Record<State, string> = {
  login: "Log in to your account",
  signup: "Create a new account",
  "forgot-password": "Forgot your password?",
  "reset-password": "Reset your password",
  "verify-email": "Email verification",
};

export function Auth({
  state,
  appearance,
  logo,
  socialLayout = "horizontal",
  additionalSignupFields,
}: { state: State } & CustomizationOptions & {
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

  const socialButtonsDirection =
    socialLayout === "vertical" ? "vertical" : "horizontal";

  return (
    <div className={styles.container} style={customStyle}>
      <div>
        {logo && <img style={logoStyle} src={logo} alt="Your Company" />}
        <h2 className={styles.headerText}>{titles[state]}</h2>
      </div>

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
        {(state === State.Login || state === State.Signup) && (
          <LoginSignupForm
            state={state}
            socialButtonsDirection={socialButtonsDirection}
            additionalSignupFields={additionalSignupFields}
          />
        )}
        {state === State.ForgotPassword && <ForgotPasswordForm />}
        {state === State.ResetPassword && <ResetPasswordForm />}
        {state === State.VerifyEmail && <VerifyEmailForm />}
      </AuthContext.Provider>
    </div>
  );
}
