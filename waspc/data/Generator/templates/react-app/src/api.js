import axios from 'axios'
import config from './config'

const api = axios.create({
  baseURL: config.apiUrl,
  withCredentials: true
})

api.interceptors.request.use(request => {
  const csrfToken = document.querySelector('meta[name="csrf-token"]')?.getAttribute('content')
  if (csrfToken) {
    request.headers['csrf-token'] = csrfToken
  }

  return request
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
