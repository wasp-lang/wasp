{{={= =}=}}
import React, { useEffect } from 'react'
import { useHistory } from 'react-router-dom'

import config from '../../config.js'
import api, { setAuthToken } from '../../api.js'

const OAuthCodeExchange = (props) => {
  const history = useHistory()
  // NOTE: Different auth methods will have different Wasp API server validation paths.
  // This helps us reuse one component for various methods (e.g., Google, Facebook, etc.).
  const validationPath = props.validationPath

  useEffect(() => {
    exchangeCodeForJwtAndRedirect(history, validationPath)
  }, [history, validationPath])

  return (
    <p>Completing login process...</p>
  )
}

async function exchangeCodeForJwtAndRedirect(history, validationPath) {
  // Take the redirect query params supplied by the external OAuth provider and
  // send them as-is to our backend, so Passport can finish the process.
  const queryParams = window.location.search
  const validationUrl = `${config.apiUrl}${validationPath}${queryParams}`

  const token = await exchangeCodeForJwt(validationUrl)
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
