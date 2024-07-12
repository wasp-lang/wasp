import { SignupForm } from "wasp/client/auth";
import { Link } from "react-router-dom";

export function SignupPage() {
  return (
    <main>
      {/** Wasp has built-in auth forms & flows, which you can customize or opt-out of, if you wish :)
       * https://wasp-lang.dev/docs/guides/auth-ui
       */}
      <SignupForm />
      <br />
      <span>
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </main>
  );
}
