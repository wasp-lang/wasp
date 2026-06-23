import { AuthLayout } from "../AuthLayout";
import { ForgotPasswordForm } from "./AuthForms";

export function RequestPasswordResetPage() {
  return (
    <AuthLayout>
      <ForgotPasswordForm />
    </AuthLayout>
  );
}
