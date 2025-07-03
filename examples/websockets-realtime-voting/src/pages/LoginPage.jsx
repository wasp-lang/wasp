import { Link } from "react-router-dom";
import { LoginForm } from "wasp/client/auth";

export function LoginPage() {
  return (
    <div>
      <LoginForm />
      <div className="mt-4 text-center">
        <Link to="/signup">Sign up instead</Link>
      </div>
    </div>
  );
}
