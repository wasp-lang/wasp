import React from 'react'
import { Link } from 'react-router-dom'

import LoginForm from '@wasp/auth/forms/Login'
import { setAuthToken } from '@wasp/api.js'

const Login = () => {
  const queryString = window.location.search;
  const urlParams = new URLSearchParams(queryString);
  const token = urlParams.get('token');

  if (token) {
    console.log('Setting token: ', token);
    setAuthToken(token);
    window.location.replace("http://localhost:3000/profile");
  }

  return (
    <>
      <LoginForm/>
      <br/>
      <span>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </span>

      <a class="button google" href="http://localhost:3001/login/federated/google">Sign in with Google</a>
    </>
  )
}

export default Login
