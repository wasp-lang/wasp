{{={= =}=}}
import React, { useEffect } from 'react'
import { useHistory } from 'react-router-dom'

import config from '../../config.js'
import api, { setAuthToken } from '../../api.js'

// After a user authenticates via an Oauth 2.0 provider, this is the page that
// the provider should redirect them to, while providing query string parameters
// that contain information needed for the API server to authenticate with the provider.
// This page forwards that information to the API server and in response get a JWT,
// which it stores on the client, therefore completing the OAuth authentication process.
const OAuthCodeExchange = (props) => {
  const history = useHistory()
  // NOTE: Different auth methods will have different Wasp API server validation paths.
  // This helps us reuse one component for various methods (e.g., Google, Facebook, etc.).
  const pathToApiServerRouteHandlingOauthRedirect = props.pathToApiServerRouteHandlingOauthRedirect

  useEffect(() => {
    exchangeCodeForJwtAndRedirect(history, pathToApiServerRouteHandlingOauthRedirect)
  }, [history, pathToApiServerRouteHandlingOauthRedirect])

  return (
    <p>Completing login process...</p>
  )
}

async function exchangeCodeForJwtAndRedirect(history, pathToApiServerRouteHandlingOauthRedirect) {
  // Take the redirect query params supplied by the external OAuth provider and
  // send them as-is to our backend, so Passport can finish the process.
  const queryParams = window.location.search
  const apiServerUrlHandlingOauthRedirect = `${config.apiUrl}${pathToApiServerRouteHandlingOauthRedirect}${queryParams}`

  const token = await exchangeCodeForJwt(apiServerUrlHandlingOauthRedirect)
  if (!token) {
    console.error('Error obtaining JWT token')
    return history.push('{= onAuthFailedRedirectTo =}')
  }

  setAuthToken(token)
  history.push('{= onAuthSucceededRedirectTo =}')
}

async function exchangeCodeForJwt(url) {
  let token = null
  try {
    const response = await api.get(url)
    token = response?.data?.token
  } catch (e) {
    console.error(e)
  }

  return token
}

export default OAuthCodeExchange
