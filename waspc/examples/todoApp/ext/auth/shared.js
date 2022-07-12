import { findOrCreateUserEntity } from '@wasp/utils.js'

async function onSignInFn(_method, _context, args) {
  return await findOrCreateUserEntity(args.profile.email)
}

export default onSignInFn
