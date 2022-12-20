import React from 'react';
import { Link } from 'react-router-dom';
import LoginForm from '@wasp/auth/forms/Login';

const LoginPage = () => {
  return (
    <main>
      <h1>Login</h1>
      {/** Wasp has built-in auth forms & flows, which you can also opt-out of, if you wish :) */}
      <LoginForm />
      <br />
      <span>
        I don't have an account yet (<Link to='/signup'>go to signup</Link>).
      </span>
    </main>
  );
};

export default LoginPage;
