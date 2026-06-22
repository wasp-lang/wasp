import { ForgotPasswordForm } from "wasp/client/auth";
import { AuthPageLayout } from "../AuthPageLayout";

export function RequestPasswordResetPage() {
  return (
    <AuthPageLayout>
      <ForgotPasswordForm />
    </AuthPageLayout>
  );
}
