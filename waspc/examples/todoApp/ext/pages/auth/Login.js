import React from 'react'
import { Link } from 'react-router-dom'

import LoginForm from '@wasp/auth/forms/Login'

const Login = () => {
  return (
    <>
      <LoginForm/>
      <br/>
      <span>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </span>
      
      <div>
        <a href="http://localhost:3001/login/federated/google">
          <img height="40" src="/images/btn_google_signin_dark_normal_web@2x.png" />
        </a>
      </div>
    </>
  )
}

export default Login
