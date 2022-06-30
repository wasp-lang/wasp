import React, { useEffect } from 'react'
import { Link } from 'react-router-dom'

import LoginForm from '@wasp/auth/forms/Login'
import api, { setAuthToken } from '@wasp/api.js'

const Login = () => {
  useEffect(() => {
    const queryString = window.location.search
    const urlParams = new URLSearchParams(queryString)
    const otpToken = urlParams.get('otpToken')

    async function fetchToken() {
      console.log('Fetching JWT from otpToken: ', otpToken)
      const response = await api.post('http://localhost:3001/otpTokenExchange', { otpToken })
      console.log(response)
      setAuthToken(response.data.token)
      window.location.replace("http://localhost:3000/profile")
    }

    if (otpToken) {
      fetchToken()
    }
  }, [])

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
