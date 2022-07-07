import React, { useEffect } from 'react'
import { useHistory } from 'react-router-dom'

import api, { setAuthToken } from '../../api.js'

// TODO: Fix hardcoded URL.
// <Redirect to="" /> vs history?

async function fetchTokenAndRedirect(otpToken, history) {
  console.log('Fetching JWT from otpToken: ', otpToken)
  try {
    const response = await api.post('http://localhost:3001/auth/external/otpTokenExchange', { otpToken })
    console.log(response)
    setAuthToken(response.data.token)
    history.push("/profile")
  } catch (e) {
    console.error('Error fetching JWT!')
    history.push("/login")
  }
}

const TokenExchange = () => {
  const history = useHistory()

  useEffect(() => {
    const queryString = window.location.search
    const urlParams = new URLSearchParams(queryString)
    const otpToken = urlParams.get('otpToken')

    if (otpToken) {
      fetchTokenAndRedirect(otpToken, history)
    } else {
      console.error('No OTP Token found!')
      history.push("/login")
    }
  }, [history])

  return (
    <p>Exchanging OTP Token for JWT...</p>
  )
}

export default TokenExchange
