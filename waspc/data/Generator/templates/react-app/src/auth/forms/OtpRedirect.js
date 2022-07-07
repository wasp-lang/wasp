import React, { useEffect } from 'react'
import { useHistory } from 'react-router-dom'

import api, { setAuthToken } from '../../api.js'

// TODO: Fix all hardcoded URLs.
// <Redirect to="" /> vs history?

const OtpRedirect = () => {
  const history = useHistory()

  useEffect(() => {
    const queryString = window.location.search
    const urlParams = new URLSearchParams(queryString)
    const otpToken = urlParams.get('otpToken')

    async function fetchToken() {
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

    if (otpToken) {
      fetchToken()
    }
  }, [])

  return (
    <p>Fetching JWT token...</p>
  )
}

export default OtpRedirect
