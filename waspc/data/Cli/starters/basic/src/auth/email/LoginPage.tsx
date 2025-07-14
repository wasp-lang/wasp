import { Link } from "react-router-dom";
import { LoginForm } from "wasp/client/auth";
import { AuthLayout } from "../AuthLayout";

export function LoginPage() {
  return (
    <AuthLayout>
      <LoginForm />
      <br />
      <span className="text-sm font-medium text-neutral-900">
        {"Don't have an account yet? "}
        <Link to="/signup" className="font-semibold underline">
          Go to signup
        </Link>
        .
      </span>
      <br />
      <span className="text-sm font-medium text-neutral-900">
        {"Forgot your password? "}
        <Link to="/request-password-reset" className="font-semibold underline">
          Reset it
        </Link>
        .
      </span>
    </AuthLayout>
  );
}
