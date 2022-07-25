import { clearLocalStorage } from '../api.js'
import { invalidateAndRemoveQueries } from '../operations/resources'

export default async function logout() {
  clearLocalStorage()

  // TODO(filip): We are currently invalidating and removing  all the queries, but
  // we should remove only the non-public, user-dependent ones.
  await invalidateAndRemoveQueries()

  // NOTE: Unfortunately, in react-router v6 you cannot use navigate outside a React context.
  // You also cannot manipulate anything like a history in the standard BrowserRouter.
  // Using HistoryRouter is a workaround, but feels like a bit of a hack.
  // In this case, I think a forced reload is an acceptable comprimise.
  // Ref: https://github.com/remix-run/react-router/issues/8264
  window.location.reload()
}
