import { Link } from "react-router-dom";
import { ResetPasswordForm } from "wasp/client/auth";
import { AuthLayout } from "../AuthLayout";

export function PasswordResetPage() {
  return (
    <AuthLayout>
      <ResetPasswordForm />
      <br />
      <span className="text-sm font-medium text-neutral-900">
        {"If everything is okay, "}
        <Link to="/login" className="font-semibold underline">
          go to login
        </Link>
        .
      </span>
    </AuthLayout>
  );
}
