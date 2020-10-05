import axios from 'axios'

import config from '../config.js'
import { setAuthToken } from '../api.js'

const login = async (email, password) => {
  // TODO(matija): make request to /auth/login/ here.
  // Make sure token is saved on the client.

  try {
    const args = { email, password }
    const response = await axios.post(config.apiUrl + '/auth/login', args)

    console.log('got token: ', response.data)

    setAuthToken(response.data.token)

  } catch (error) {
    // TODO(matija): refine this, extract maybe from callOperation
    console.log(error)
    throw error
  }
}

export default login
