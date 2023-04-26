import { Link } from 'react-router-dom';
import { LoginForm } from '@wasp/auth/forms/Login';

export function LoginPage() {
  return (
    <main>
      <h1>Login</h1>
      {/** Wasp has built-in auth forms & flows, which you can customize or opt-out of, if you wish :)
       * https://wasp-lang.dev/docs/guides/auth-ui
       */}
      <LoginForm />
      <br />
      <span>
        I don't have an account yet (<Link to='/signup'>go to signup</Link>).
      </span>
    </main>
  );
};
