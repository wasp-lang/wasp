import { Link } from "react-router";
import { LoginForm } from "wasp/client/auth";
import { AuthLayout } from "../AuthLayout";

export function LoginPage() {
  return (
    <AuthLayout>
      <LoginForm />
      <br />
      <span className="text-sm">
        Don&apos;t have an account yet?{" "}
        <Link to="/signup" className="font-bold underline">
          Go to signup
        </Link>
        .
      </span>
      <br />
      <span className="text-sm">
        Forgot your password?{" "}
        <Link to="/request-password-reset" className="font-bold underline">
          Reset it
        </Link>
        .
      </span>
    </AuthLayout>
  );
}
