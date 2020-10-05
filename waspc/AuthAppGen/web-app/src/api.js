import axios from 'axios'
import config from './config'

const api = axios.create({
  baseURL: config.apiUrl,
  timeout: parseInt(process.env.REACT_APP_AXIOS_TIMEOUT) // Undefined is fine, will use default value.
})

const WASP_APP_AUTH_TOKEN_NAME = "waspAppAuthToken"

let authToken = null
if (window.localStorage) {
  authToken = window.localStorage.getItem(WASP_APP_AUTH_TOKEN_NAME)
}

export const setAuthToken = (token) => {
  if (typeof token !== 'string') {
    throw Error(`Token must be a string, but it was: {${typeof token}} ${token}.`)
  }
  authToken = token
  window.localStorage && window.localStorage.setItem(WASP_APP_AUTH_TOKEN_NAME, token)
}

export const clearAuthToken = () => {
  authToken = undefined
}

api.interceptors.request.use(request => {
  if (authToken) {
    request.headers['Authorization'] = `Bearer ${authToken}`
  }
  return request
})

export default api
