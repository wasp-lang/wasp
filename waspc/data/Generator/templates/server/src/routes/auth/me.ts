import { createMeHandler } from '@wasp.sh/lib-auth/node'
import { serialize } from 'wasp/core/serialization'

export default createMeHandler(serialize)
