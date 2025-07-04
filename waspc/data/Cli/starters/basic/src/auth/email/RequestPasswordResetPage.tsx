import { ForgotPasswordForm } from "wasp/client/auth";
import { AuthLayout } from "../AuthLayout";

export function RequestPasswordResetPage() {
  return (
    <AuthLayout>
      <ForgotPasswordForm />
    </AuthLayout>
  );
}
