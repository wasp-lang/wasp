import { Link } from "react-router";
import { VerifyEmailForm } from "wasp/client/auth";

export default function EmailVerification() {
  return (
    <div style={{ maxWidth: "400px", margin: "0 auto" }}>
      <VerifyEmailForm />
      <br />
      <span>
        If everything is okay, <Link to="/login">go to login</Link>.
      </span>
    </div>
  );
}
