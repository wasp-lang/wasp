import { LoginForm } from "wasp/client/auth";
import { Link } from "react-router-dom";

export function LoginPage() {
  return (
    <main>
      {/** Wasp has built-in auth forms & flows, which you can customize or opt-out of, if you wish :)
       * https://wasp.sh/docs/guides/auth-ui
       */}
      <LoginForm />
      <br />
      <span>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </span>
    </main>
  );
}
