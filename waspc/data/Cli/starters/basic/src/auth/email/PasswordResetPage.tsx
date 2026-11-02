import { Link } from "react-router";
import { ResetPasswordForm } from "wasp/client/auth";
import { AuthLayout } from "../AuthLayout";

export function PasswordResetPage() {
  return (
    <AuthLayout>
      <ResetPasswordForm />
      <p className="text-sm">
        If everything is okay,{" "}
        <Link to="/login" className="font-bold underline">
          go to login
        </Link>
        .
      </p>
    </AuthLayout>
  );
}
