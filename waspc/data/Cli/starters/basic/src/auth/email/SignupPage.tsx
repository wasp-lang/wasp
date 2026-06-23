import { Link } from "react-router";
import { AuthLayout } from "../AuthLayout";
import { SignupForm } from "./AuthForms";

export function SignupPage() {
  return (
    <AuthLayout>
      <SignupForm />
      <br />
      <span className="text-sm font-medium text-neutral-900">
        Already have an account?{" "}
        <Link to="/login" className="font-semibold underline">
          Go to login
        </Link>
        .
      </span>
    </AuthLayout>
  );
}
