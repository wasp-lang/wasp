// The uniform client auth surface, the same for every provider mix. Provider
// UI (login forms, sign-in buttons) comes from each auth package's own client
// entry (`@wasp.sh/auth/client` for Wasp's own auth).
export {
  default as useAuth,
  getMe,
} from '../../auth/useAuth'

export { default as logout } from '../../auth/logout'

export { resumeSession, loginWithAuthProvider } from './providers'
