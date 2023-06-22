{{={= =}=}}
import React, { useEffect, useRef } from 'react'
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

  // We are using a ref to prevent sending the OAuth token twice in development.
  // Since React 18 and using their StrictMode, useEffect is called twice in development.

  // Fixing it this way is not recommended by the docs, but they don't offer any alternatives
  // for this particular use case (oauth redirect page):
  // https://react.dev/learn/synchronizing-with-effects#how-to-handle-the-effect-firing-twice-in-development
  const firstRender = useRef(true)
  useEffect(() => {
    if (!firstRender.current) {
      return
    }
    // NOTE: Different auth methods will have different Wasp API server validation paths.
    // This helps us reuse one component for various methods (e.g., Google, Facebook, etc.).
    const apiServerUrlHandlingOauthRedirect = constructOauthRedirectApiServerUrl(pathToApiServerRouteHandlingOauthRedirect)

    exchangeCodeForJwtAndRedirect(history, apiServerUrlHandlingOauthRedirect)
    return () => {
      firstRender.current = false
    }
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
    history.push('{= onAuthSucceededRedirectTo =}')
  } else {
    console.error('Error obtaining JWT token')
    history.push('{= onAuthFailedRedirectTo =}')
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
