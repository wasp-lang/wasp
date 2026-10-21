import { createLogoutHandler } from '@wasp.sh/lib-auth/node'
import { sessions } from 'wasp/server/auth/session'

export default createLogoutHandler(sessions)
