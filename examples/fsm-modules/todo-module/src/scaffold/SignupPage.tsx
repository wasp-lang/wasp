import { SignupForm } from "wasp/client/auth";
import { Link } from "react-router";

export function SignupPage() {
  return (
    <div style={{ maxWidth: "400px", margin: "0 auto" }}>
      <SignupForm />
      <br />
      <span>
        Already have an account? <Link to="/login">go to login</Link>.
      </span>
    </div>
  );
}
