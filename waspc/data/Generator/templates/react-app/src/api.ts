import axios, { type AxiosError } from 'axios'
import config from './config'
import { storage } from './storage'

const api = axios.create({
  baseURL: config.apiUrl,
})

const WASP_APP_AUTH_TOKEN_NAME = 'authToken'

let authToken: string | undefined = storage.get(WASP_APP_AUTH_TOKEN_NAME)

export const setAuthToken = (token: unknown): void => {
  if (typeof token !== 'string') {
    throw Error(
      `Token must be a string, but it was: {${typeof token}} ${token}.`
    )
  }
  authToken = token
  storage.set(WASP_APP_AUTH_TOKEN_NAME, token)
}

export const clearAuthToken = (): void => {
  authToken = undefined
  storage.remove(WASP_APP_AUTH_TOKEN_NAME)
}

export const removeLocalUserData = (): void => {
  authToken = undefined

  storage.clear()
}

api.interceptors.request.use((request) => {
  if (authToken) {
    request.headers['Authorization'] = `Bearer ${authToken}`
  }
  return request
})

api.interceptors.response.use(undefined, (error) => {
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
export const handleApiError = (error: AxiosError): void => {
  if (error?.response) {
    // If error came from HTTP response, we capture most informative message
    // and also add .statusCode information to it.
    // If error had JSON response, we assume it is of format { message, data } and
    // add that info to the error.
    // TODO: We might want to use HttpError here instead of just Error, since
    //   HttpError is also used on server to throw errors like these.
    //   That would require copying HttpError code to web-app also and using it here.
    const responseJson = error.response?.data as {
      message?: string
      data?: unknown
    }
    const responseStatusCode = error.response.status
    const e: WaspError = new Error(responseJson?.message || error.message)
    e.statusCode = responseStatusCode
    e.data = responseJson?.data
    throw e
  } else {
    // If any other error, we just propagate it.
    throw error
  }
}

type WaspError = Error & {
  statusCode?: number
  data?: unknown
}

export default api
