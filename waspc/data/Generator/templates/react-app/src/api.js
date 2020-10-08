import axios from 'axios'
import config from './config'

const api = axios.create({
  baseURL: config.apiUrl,
  timeout: parseInt(process.env.REACT_APP_AXIOS_TIMEOUT) // Undefined is fine, will use default value.
})

// TODO(matija): this can be generated according to the app's name.
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
  window.localStorage && window.localStorage.removeItem(WASP_APP_AUTH_TOKEN_NAME)
}

export const clearLocalStorage = () => {
  // TODO(matija): this is a bit clumsy, we have this local variable so we also have to
  // manage it, besides local storage. Can we get rid of it?
  authToken = undefined

  window.localStorage && window.localStorage.clear()
}

api.interceptors.request.use(request => {
  if (authToken) {
    request.headers['Authorization'] = `Bearer ${authToken}`
  }
  return request
})

export const handleApiError = (error) => {
  if (error?.response) {
    // If error came from HTTP response, we capture most informative message
    // and also add .statusCode information to it.
    // If error had JSON response, we assume it is of format { message, data } and
    // add that info to the error.
    // TODO: We might want to use HttpError here instead of just Error, since
    //   HttpError is also used on server to throw errors like these.
    //   That would require copying HttpError code to web-app also and using it here.
    const responseJson = error.response?.data
    const responseStatusCode = error.response.status
    const e = new Error(responseJson?.message || error.message)
    e.statusCode = responseStatusCode
    e.data = responseJson?.data
    throw e
  } else {
    // If any other error, we just propagate it.
    throw error
  }
}

export default api
