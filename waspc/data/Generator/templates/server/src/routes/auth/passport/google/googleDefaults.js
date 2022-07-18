import { findOrCreateUserEntity } from '../../../../core/auth.js'

// Default implementation if a user does not supply their own `auth.methods.google.configFn`.
// If they do, this function will not be invoked.
export function configFn() {
  return {
    clientId: process.env['GOOGLE_CLIENT_ID'],
    clientSecret: process.env['GOOGLE_CLIENT_SECRET'],
  }
}

// Default implementation if a user does not supply their own `auth.methods.google.onSignInFn`.
// If they do, this function will not be invoked.
export async function onSignInFn(_context, args) {
  const email = args.profile.emails[0].value
  return await findOrCreateUserEntity(email)
}
