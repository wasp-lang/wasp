import { LoginForm } from "@wasp/auth/forms/Login";
import { Link } from "react-router-dom";

export function LoginPage() {
  return (
    <div>
      <LoginForm />
      <div className="text-center mt-4">
        <Link to="/signup">Sign up instead</Link>
      </div>
    </div>
  );
}
