import { clearLocalStorage } from '../api.js'
import { invalidateAndClearQueries } from '../operations/resources'

export default function logout() {
  clearLocalStorage()
  invalidateAndClearQueries()
}
