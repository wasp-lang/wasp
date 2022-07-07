import React, { useEffect } from 'react'
import { Link } from 'react-router-dom'

import LoginForm from '@wasp/auth/forms/Login'
import { Google } from '@wasp/auth/forms/SocialLoginButtons'
import api, { setAuthToken } from '@wasp/api.js'

const Login = () => {
  return (
    <>
      <LoginForm/>
      <br/>
      <span>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </span>

      <div>
        <Google/>
      </div>
    </>
  )
}

export default Login
