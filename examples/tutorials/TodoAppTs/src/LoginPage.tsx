import { Link } from "react-router";
import { LoginForm } from "wasp/client/auth";
import "./Main.css";

export const LoginPage = () => {
  return (
    <main className="card">
      <LoginForm />
      <span>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </span>
    </main>
  );
};
