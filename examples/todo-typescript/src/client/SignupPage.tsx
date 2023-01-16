import { Link } from 'react-router-dom';
import SignupForm from '@wasp/auth/forms/Signup';

export function SignupPage() {
  return (
    <main>
      <h1>Sign Up</h1>
      {/** Wasp has built-in auth forms & flows, which you can also opt-out of, if you wish :) */}
      <SignupForm />
      <br />
      <span>
        I already have an account (<Link to='/login'>go to login</Link>).
      </span>
    </main>
  );
};
