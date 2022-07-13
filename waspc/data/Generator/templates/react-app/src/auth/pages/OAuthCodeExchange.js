{{={= =}=}}
import React, { useEffect } from 'react'
import { useHistory } from 'react-router-dom'

import config from '../../config.js'
import api, { setAuthToken } from '../../api.js'

const OAuthCodeExchange = (props) => {
  const history = useHistory()
  const validationPath = props.validationPath

  useEffect(() => {
    exchangeCodeForJwtAndRedirect(history, validationPath)
  }, [history, validationPath])

  return (
    <p>Completing login process...</p>
  )
}

async function exchangeCodeForJwtAndRedirect(history, validationPath) {
  const queryParams = window.location.search

  const token = await exchangeCodeForJwt(queryParams, validationPath)
  if (token) {
    setAuthToken(token)
    return history.push('{= onAuthSucceededRedirectTo =}')
  }

  console.error('Error obtaining JWT token')
  history.push('{= onAuthFailedRedirectTo =}')
}

async function exchangeCodeForJwt(queryParams, validationPath) {
  const validationUrl = `${config.apiUrl}${validationPath}${queryParams}`

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
