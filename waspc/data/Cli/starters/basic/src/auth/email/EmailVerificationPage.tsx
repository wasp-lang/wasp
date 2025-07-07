import { Link } from "react-router-dom";
import { VerifyEmailForm } from "wasp/client/auth";
import { AuthLayout } from "../AuthLayout";

export function EmailVerificationPage() {
  return (
    <AuthLayout>
      <VerifyEmailForm />
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
