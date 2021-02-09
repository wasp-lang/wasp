import axios from 'axios'
import config from './config'

const api = axios.create({
  baseURL: config.apiUrl,
})

const WASP_APP_AUTH_TOKEN_NAME = "authToken"

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
  authToken = undefined

  window.localStorage && window.localStorage.clear()
}

api.interceptors.request.use(request => {
  if (authToken) {
    request.headers['Authorization'] = `Bearer ${authToken}`
  }
  return request
})

api.interceptors.response.use(undefined, error => {
  if (error.response?.status === 401) {
    clearAuthToken()
  }
  return Promise.reject(error)
})

/**
 * Takes an error returned by the app's API (as returned by axios), and transforms into a more
 * standard format to be further used by the client. It is also assumed that given API
 * error has been formatted as implemented by HttpError on the server.
 */
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
