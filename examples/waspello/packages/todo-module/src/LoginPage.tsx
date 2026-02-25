import { LoginForm } from "wasp/client/auth";
import { Link } from "react-router";

export function LoginPage() {
  return (
    <div style={{ maxWidth: "400px", margin: "0 auto" }}>
      <LoginForm />
      <br />
      <span>
        Don't have an account yet? <Link to="/signup">go to signup</Link>.
      </span>
    </div>
  );
}
