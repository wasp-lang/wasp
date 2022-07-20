import { upsertUserWithRandomPassword } from '../../../../core/auth.js'

// Default implementation if there is no `auth.methods.google.configFn`.
export function configFn() {
  const clientId = process.env['GOOGLE_CLIENT_ID']
  const clientSecret = process.env['GOOGLE_CLIENT_SECRET']

  if (!clientId) {
    throw new Error("Missing GOOGLE_CLIENT_ID environment variable.")
  }

  if (!clientSecret) {
    throw new Error("Missing GOOGLE_CLIENT_SECRET environment variable.")
  }

  return { clientId, clientSecret, scope: ['email', 'profile'] }
}

// Default implementation if there is no `auth.methods.google.onSignInFn`.
export async function onSignInFn(_context, args) {
  const email = args.profile.emails[0].value
  // NOTE: For security reasons, `upsertUserWithRandomPassword` updates existing User
  // passwords to a random string, thus disabling future `emailAndPassword` logins.
  // Once Wasp adds email validation and password reset, we will no longer do this.
  return await upsertUserWithRandomPassword(email)
}
