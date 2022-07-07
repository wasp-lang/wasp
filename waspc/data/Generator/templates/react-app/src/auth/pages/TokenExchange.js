import React, { useEffect } from 'react'
import { useHistory } from 'react-router-dom'

import config from '../../config.js'
import api, { setAuthToken } from '../../api.js'

const TokenExchange = () => {
  const history = useHistory()

  useEffect(() => {
    exchangeTokenAndRedirect(history)
  }, [history])

  return (
    <p>Exchanging OTP Token for JWT...</p>
  )
}

async function exchangeTokenAndRedirect(history) {
  const queryString = window.location.search
  const urlParams = new URLSearchParams(queryString)
  const otpToken = urlParams.get('otpToken')

  // TODO: Make use of redirect paths instead of "/profile" and "/login".
  if (otpToken) {
    const token = await exchangeToken(otpToken, history)
    if (token) {
      setAuthToken(token)
      return history.push("/profile")
    }
  }

  console.error('Error obtaining JWT token')
  history.push("/login")
}

async function exchangeToken(otpToken) {
  let token = null

  try {
    const response = await api.post(`${config.apiUrl}/auth/external/otpTokenExchange`, { otpToken })
    token = response?.data?.token
  } catch (e) {
    console.error(e)
  }

  return token
}

export default TokenExchange
