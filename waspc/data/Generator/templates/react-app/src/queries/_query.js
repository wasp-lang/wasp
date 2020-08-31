{{={= =}=}}
import axios from 'axios'

import config from '../config.js'

const {= queryFnName =} = async (args) => {
  try {
    const response = await axios.post(config.apiUrl + '/{= queryRoute =}', args)
    return response.data
  } catch (error) {
    // TODO: This is a really crude error handling for now, and we should look into improving it,
    //   once we figure out what we need. We should start from the server side probably.
    const e = new Error(error.message)
    if (error?.response?.data) {
      e.data = error.response.data
    }
    throw e
  }
}

{= queryFnName =}.useQueryKey = '{= queryRoute =}'

export default {= queryFnName =}
