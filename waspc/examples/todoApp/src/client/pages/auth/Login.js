import React from 'react'
import { Link } from 'react-router-dom'

import LoginForm from '@wasp/auth/forms/Login'
// import { SignInButton as GoogleSignInButton } from '@wasp/auth/helpers/Google'
// import { SignInButton as GitHubSignInButton } from '@wasp/auth/helpers/GitHub'

const Login = () => {
  return (
    <>
      <LoginForm/>
      <br/>
      <span>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </span>

      {/* <div>
        <GoogleSignInButton/>
        <GitHubSignInButton/>
      </div> */}
    </>
  )
}

export default Login
