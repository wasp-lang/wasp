import config from '../config.js'
import { removeQueries } from '../operations/resources'
import api, { setAuthToken, handleApiError } from '../api.js'

export default async function login(email, password) {
  try {
    const args = { email, password }
    const response = await api.post(config.apiUrl + '/auth/login', args)

    setAuthToken(response.data.token)
    // This isn't really neccessary because we remove all private queries after
    // logout, but we do it to be extra safe.
    // 
    // For example, in future versions, users might be able to get to an SPA
    // login page while there's an active session. This code will prevent data
    // leaks in such cases.
    //
    // TODO(filip): We are currently removing all the queries, but we should
    // remove only non-public, user-dependent queries - public queries are
    // expected not to change in respect to the currently logged in user.
    await removeQueries()
  } catch (error) {
    handleApiError(error)
  }
}
