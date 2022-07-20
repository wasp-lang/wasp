{{={= =}=}}
import React, { useEffect } from 'react'
import { useHistory } from 'react-router-dom'

import config from '../../config.js'
import api, { setAuthToken } from '../../api.js'

// This component is rendered in response to an OAuth 2.0 redirect request,
// where it takes all of the query string parameters and sends them to the Wasp
// API server to complete the OAuth process. Upon completion, we will
// obtain a JWT and redirect to the auth success path.
const OAuthCodeExchange = (props) => {
  const history = useHistory()
  // NOTE: Different auth methods will have different Wasp API server validation paths.
  // This helps us reuse one component for various methods (e.g., Google, Facebook, etc.).
  const handleOauthRedirectPath = props.handleOauthRedirectPath

  useEffect(() => {
    exchangeCodeForJwtAndRedirect(history, handleOauthRedirectPath)
  }, [history, handleOauthRedirectPath])

  return (
    <p>Completing login process...</p>
  )
}

async function exchangeCodeForJwtAndRedirect(history, handleOauthRedirectPath) {
  // Take the redirect query params supplied by the external OAuth provider and
  // send them as-is to our backend, so Passport can finish the process.
  const queryParams = window.location.search
  const handleOauthRedirectUrl = `${config.apiUrl}${handleOauthRedirectPath}${queryParams}`

  const token = await exchangeCodeForJwt(handleOauthRedirectUrl)
  if (!token) {
    console.error('Error obtaining JWT token')
    return history.push('{= onAuthFailedRedirectTo =}')
  }

  setAuthToken(token)
  history.push('{= onAuthSucceededRedirectTo =}')
}

async function exchangeCodeForJwt(validationUrl) {
  let token = null
  try {
    const response = await api.get(validationUrl)
    token = response?.data?.token
  } catch (e) {
    console.error(e)
  }

  return token
}

export default OAuthCodeExchange
