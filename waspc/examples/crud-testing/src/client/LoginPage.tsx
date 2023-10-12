import { LoginForm } from "@wasp/auth/forms/Login";
import { Link } from "@wasp/router";

export const LoginPage = () => {
  return (
    <div className="container">
      <main>
        <h1>Login</h1>
        <LoginForm />
        <div>
          <Link to="/signup">Sign up</Link>
        </div>
      </main>
    </div>
  );
};
