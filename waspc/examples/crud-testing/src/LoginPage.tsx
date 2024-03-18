import { Link } from "wasp/client/router";
import { LoginForm } from "wasp/client/auth";

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
