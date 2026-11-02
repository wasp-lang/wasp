import { Link } from "react-router";
import { SignupForm } from "wasp/client/auth";
import "./Main.css";

export const SignupPage = () => {
  return (
    <main className="card">
      <SignupForm />
      <span>
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </main>
  );
};
