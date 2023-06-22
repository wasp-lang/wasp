import React, { useEffect } from 'react'
import { useHistory } from 'react-router-dom'

import config from '../../config.js'
import api from '../../api'
import { initSession } from '../helpers/user'

// After a user authenticates via an Oauth 2.0 provider, this is the page that
// the provider should redirect them to, while providing query string parameters
// that contain information needed for the API server to authenticate with the provider.
// This page forwards that information to the API server and in response get a JWT,
// which it stores on the client, therefore completing the OAuth authentication process.
export default function OAuthCodeExchange({ pathToApiServerRouteHandlingOauthRedirect }) {
  const history = useHistory()

  useEffect(() => {
    // NOTE: Different auth methods will have different Wasp API server validation paths.
    // This helps us reuse one component for various methods (e.g., Google, Facebook, etc.).
    const apiServerUrlHandlingOauthRedirect = constructOauthRedirectApiServerUrl(pathToApiServerRouteHandlingOauthRedirect)

    exchangeCodeForJwtAndRedirect(history, apiServerUrlHandlingOauthRedirect)
  }, [history, pathToApiServerRouteHandlingOauthRedirect])

  return (
    <p>Completing login process...</p>
  )
}

function constructOauthRedirectApiServerUrl(pathToApiServerRouteHandlingOauthRedirect) {
  // Take the redirect query params supplied by the external OAuth provider and
  // send them as-is to our backend, so Passport can finish the process.
  const queryParams = window.location.search
  return `${config.apiUrl}${pathToApiServerRouteHandlingOauthRedirect}${queryParams}`
}

async function exchangeCodeForJwtAndRedirect(history, apiServerUrlHandlingOauthRedirect) {
  const token = await exchangeCodeForJwt(apiServerUrlHandlingOauthRedirect)

  if (token !== null) {
    await initSession(token)
    history.push('/')
  } else {
    console.error('Error obtaining JWT token')
    history.push('/login')
  }
}

async function exchangeCodeForJwt(url) {
  try {
    const response = await api.get(url)
    return response?.data?.token || null
  } catch (e) {
    console.error(e)
    return null
  }
}
