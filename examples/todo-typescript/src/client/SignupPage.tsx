import { Link } from 'react-router-dom';
import { SignupForm } from '@wasp/auth/forms/Signup';

export function SignupPage() {
  return (
    <main>
      <h1>Sign Up</h1>
      {/** Wasp has built-in auth forms & flows, which you can customize or opt-out of, if you wish :)
       * https://wasp-lang.dev/docs/guides/auth-ui
       */}
      <SignupForm />
      <br />
      <span>
        I already have an account (<Link to='/login'>go to login</Link>).
      </span>
    </main>
  );
};
