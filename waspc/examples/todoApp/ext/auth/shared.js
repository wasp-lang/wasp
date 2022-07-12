import { findOrCreateUserEntity, GOOGLE_AUTH_METHOD } from '@wasp/core/auth.js'

async function onSignInFn(method, _context, args) {
  let email

  // TODO: Add helper for this.
  switch (method) {
    case GOOGLE_AUTH_METHOD:
      email = args.profile.emails[0].value
      break;
    default:
      throw `Unknown auth method: ${method}`
  }

  return await findOrCreateUserEntity(email)
}

export default onSignInFn
