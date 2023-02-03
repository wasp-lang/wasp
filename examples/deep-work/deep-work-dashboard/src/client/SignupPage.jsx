import React from 'react';
import { Link, useHistory } from 'react-router-dom';
import useAuth from '@wasp/auth/useAuth';
import SignupForm from '@wasp/auth/forms/Signup';

const SignupPage = () => {
  const { data: user } = useAuth();
  const history = useHistory();

  if (user) history.push('/');

  return (
    <main>
      <div>
        <SignupForm />

      </div>
        <p>
          Note: Make sure that your username matches your Discord Display name
        </p>
          
        <em>
          Take me back to <Link to='/'>Login</Link>
          </em>
    </main>
  );

};
export default SignupPage;
