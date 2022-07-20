import { findOrCreateUserEntity } from '../../../../core/auth.js'

// Default implementation if there is no `auth.methods.google.configFn`.
// If they do, this function will not be invoked.
export function configFn() {
  return {
    clientId: process.env['GOOGLE_CLIENT_ID'],
    clientSecret: process.env['GOOGLE_CLIENT_SECRET'],
  }
}

// Default implementation if there is no `auth.methods.google.onSignInFn`.
// If they do, this function will not be invoked.
export async function onSignInFn(_context, args) {
  const email = args.profile.emails[0].value
  // NOTE: For security reasons, `findOrCreateUserEntity` updates existing User
  // passwords to a random string, thus disabling future `emailAndPassword` logins.
  // Once Wasp adds email validation and password reset, we will no longer do this.
  return await findOrCreateUserEntity(email)
}
