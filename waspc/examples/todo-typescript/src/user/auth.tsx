//import { ResetPasswordForm } from "wasp/auth/forms/ResetPassword";
import { LoginForm } from "wasp/auth/forms/Login";
//import { VerifyEmailForm } from "wasp/auth/forms/VerifyEmail";
import { SignupForm } from "wasp/auth/forms/Signup";
//import { ForgotPasswordForm } from "wasp/auth/forms/ForgotPassword";
import { Link } from "react-router-dom";

export function SignupPage() {
  return (
    <main>
      {/** Wasp has built-in auth forms & flows, which you can customize or opt-out of, if you wish :)
       * https://wasp-lang.dev/docs/guides/auth-ui
       */}
      <SignupForm
        additionalFields={[
          {
            type: "input",
            label: "Address",
            name: "address",
            validations: {
              required: "Address is required",
            },
          },
        ]}
      />
      <br />
      <span>
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </main>
  );
}

export function LoginPage() {
  return (
    <main>
      {/** Wasp has built-in auth forms & flows, which you can customize or opt-out of, if you wish :)
       * https://wasp-lang.dev/docs/guides/auth-ui
       */}
      <LoginForm />
      <br />
      <span>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </span>
    </main>
  );
}

/* export function RequestPasswordResetPage() {
*   return <ForgotPasswordForm />;
* }
* 
* export function PasswordResetPage() {
*   return <ResetPasswordForm />;
* }
* 
* export function EmailVerificationPage() {
*   return <VerifyEmailForm />;
* } */
