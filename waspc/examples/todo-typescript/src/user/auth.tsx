// import { ResetPasswordForm } from "wasp/auth/forms/ResetPassword";
import { LoginForm } from "wasp/auth/forms/Login";
// import { VerifyEmailForm } from "wasp/auth/forms/VerifyEmail";
import { SignupForm } from "wasp/auth/forms/Signup";
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from "wasp/auth/forms/internal/Form";
// import {
//   SignInButton as GitHubSignInButton,
//   signInUrl as gitHubSignInUrl,
// } from "wasp/auth/helpers/GitHub";
// import {
//   SignInButton as GoogleSignInButton,
//   signInUrl as googleSignInUrl,
// } from "wasp/auth/helpers/Google";

// import { ForgotPasswordForm } from "wasp/auth/forms/ForgotPassword";
import { Link, routes } from "wasp/router";

export function SignupPage() {
  return (
    <main>
      {/** Wasp has built-in auth forms & flows, which you can customize or opt-out of, if you wish :)
       * https://wasp-lang.dev/docs/guides/auth-ui
       */}
      <SignupForm
        additionalFields={({ register, formState: { errors } }) => {
          return (
            <FormItemGroup>
              <FormLabel>Address</FormLabel>
              <FormInput
                {...register("address", {
                  required: "Address is required",
                })}
              />
              {errors.address && (
                <FormError>{errors.address.message}</FormError>
              )}
            </FormItemGroup>
          );
        }}
      />
      {/* <div style={{ marginTop: "1rem" }}>
        Extra Github Button: <GitHubSignInButton /> with link {gitHubSignInUrl}
      </div>
      <div style={{ marginTop: "1rem" }}>
        Extra Google Button: <GoogleSignInButton /> with link {googleSignInUrl}
      </div> */}
      <br />
      <span>
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
      <span>The link to the login page is {routes.LoginRoute.build()}.</span>
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

// export function RequestPasswordResetPage() {
//   return <ForgotPasswordForm />;
// }

// export function PasswordResetPage() {
//   return <ResetPasswordForm />;
// }

// export function EmailVerificationPage() {
//   return <VerifyEmailForm />;
// }
