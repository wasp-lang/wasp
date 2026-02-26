import { Link } from "react-router";
import { ResetPasswordForm } from "wasp/client/auth";

export default function PasswordReset() {
  return (
    <div style={{ maxWidth: "400px", margin: "0 auto" }}>
      <ResetPasswordForm />
      <br />
      <span>
        If everything is okay, <Link to="/login">go to login</Link>.
      </span>
    </div>
  );
}
