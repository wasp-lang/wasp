import { generateAvailableDictionaryUsername } from '../../../../core/auth.js'

// Default implementation if there is no `auth.methods.google.configFn`.
export function configFn() {
  const clientID = process.env['GOOGLE_CLIENT_ID']
  const clientSecret = process.env['GOOGLE_CLIENT_SECRET']

  if (!clientID) {
    throw new Error("Missing GOOGLE_CLIENT_ID environment variable.")
  }

  if (!clientSecret) {
    throw new Error("Missing GOOGLE_CLIENT_SECRET environment variable.")
  }

  return { clientID, clientSecret, scope: ['profile'] }
}

// Default implementation if there is no `auth.methods.google.getUserFieldsFn`.
export async function getUserFieldsFn(_context, _args) {
  const username = await generateAvailableDictionaryUsername()
  return { username }
}
