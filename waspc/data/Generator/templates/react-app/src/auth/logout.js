import { invalidateAndRemoveQueries } from '../operations/resources'
import config from '../config.js'
import api, { handleApiError } from '../api.js'

export default async function logout() {
  try {
    await api.post(config.apiUrl + '/auth/logout').finally(async () => {
      window.sendMessage({message: "clearCookies"})

      // TODO(filip): We are currently invalidating and removing  all the queries, but
      // we should remove only the non-public, user-dependent ones.
      await invalidateAndRemoveQueries()
    })
  } catch (error) {
    handleApiError(error)
  }
}
