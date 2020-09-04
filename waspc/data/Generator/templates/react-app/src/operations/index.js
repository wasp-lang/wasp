{{={= =}=}}
import axios from 'axios'

import config from '../config.js'

export const callOperation = async (operationRoute, args) => {
  try {
    const response = await axios.post(config.apiUrl + '/' + operationRoute, args)
    return response.data
  } catch (error) {
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
}
