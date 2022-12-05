import React from 'react'
import { Link } from 'react-router-dom'

import LoginForm from '@wasp/auth/forms/Login'
import { GoogleSignInButton } from '@wasp/auth/buttons/Google'
import { GithubSignInButton } from '@wasp/auth/buttons/Github'

const Login = () => {
  return (
    <>
      <LoginForm/>
      <br/>
      <span>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </span>

      <div>
        <GoogleSignInButton/>
      </div>
      <div>
        <GithubSignInButton/>
      </div>
    </>
  )
}

export default Login
