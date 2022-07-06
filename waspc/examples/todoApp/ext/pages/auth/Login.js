import React, { useEffect } from 'react'
import { Link } from 'react-router-dom'

import LoginForm from '@wasp/auth/forms/Login'
import api, { setAuthToken } from '@wasp/api.js'

// TODO: Fix all hardcoded URLs.

const Login = () => {
  // TODO: Move to a different component.
  useEffect(() => {
    const queryString = window.location.search
    const urlParams = new URLSearchParams(queryString)
    const otpToken = urlParams.get('otpToken')

    async function fetchToken() {
      console.log('Fetching JWT from otpToken: ', otpToken)
      try {
        const response = await api.post('http://localhost:3001/otpTokenExchange', { otpToken })
        console.log(response)
        setAuthToken(response.data.token)
        window.location.replace("http://localhost:3000/profile")
      } catch (e) {
        console.error('Error fetching JWT!')
        window.location.replace("http://localhost:3000/login")
      }
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

      <div>
        <a href="http://localhost:3001/login/federated/google">
          <img height="40" src="/images/btn_google_signin_dark_normal_web@2x.png" />
        </a>
      </div>
    </>
  )
}

export default Login
