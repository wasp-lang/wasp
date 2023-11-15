import { SignupForm } from "@wasp/auth/forms/Signup";
import { LoginForm } from "@wasp/auth/forms/Login";
import { VerifyEmailForm } from "@wasp/auth/forms/VerifyEmail";
import { ForgotPasswordForm } from "@wasp/auth/forms/ForgotPassword";
import { ResetPasswordForm } from "@wasp/auth/forms/ResetPassword";

export function Signup() {
  return <SignupForm />;
}

export function Login() {
  return <LoginForm />;
}

export function RequestPasswordReset() {
  return <ForgotPasswordForm />;
}

export function PasswordReset() {
  return <ResetPasswordForm />;
}

export function EmailVerification() {
  return <VerifyEmailForm />;
}
