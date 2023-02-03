import React from 'react';
import { Link, useHistory } from 'react-router-dom';
import useAuth from '@wasp/auth/useAuth';
import LoginForm from '@wasp/auth/forms/Login';

const LoginPage = () => {
  const { data: user } = useAuth();
  const history = useHistory();

  if (user) history.push('/');

  return (
    <main>
      <div >
        <LoginForm />
      </div>
      <br/>
      <em>
        I want to <Link to='/signup'>register</Link>
      </em>
    </main>
  );
};
export default LoginPage;
