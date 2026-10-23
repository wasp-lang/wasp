import { Link } from "react-router";
import { VerifyEmailForm } from "wasp/client/auth";
import { AuthLayout } from "../AuthLayout";

export function EmailVerificationPage() {
  return (
    <AuthLayout>
      <VerifyEmailForm />
      <br />
      <span className="text-sm">
        If everything is okay,{" "}
        <Link to="/login" className="font-bold underline">
          go to login
        </Link>
        .
      </span>
    </AuthLayout>
  );
}
