import { generateAvailableUsername } from '../../utils.js'

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

  return { clientId, clientSecret, scope: ['profile'] }
}

// Default implementation if there is no `auth.methods.google.onSignInFn`.
export async function firstSignInConfig(_context, args) {
  const username = await generateAvailableUsername()
  // TODO: rename `email` to `username` after merge.
  return { userFields: { email: username }, redirectPath: "/profile" }
}
