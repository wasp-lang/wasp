import { generateAvailableDictionaryUsername } from '../../../../core/auth.js'

// Default implementation if there is no `auth.methods.github.configFn`.
export function configFn() {
  const clientID = process.env['GITHUB_CLIENT_ID']
  const clientSecret = process.env['GITHUB_CLIENT_SECRET']

  if (!clientID) {
    throw new Error("Missing GITHUB_CLIENT_ID environment variable.")
  }

  if (!clientSecret) {
    throw new Error("Missing GITHUB_CLIENT_SECRET environment variable.")
  }

  return { clientID, clientSecret, scope: [] }
}

// Default implementation if there is no `auth.methods.github.getUserFieldsFn`.
export async function getUserFieldsFn(_context, _args) {
  const username = await generateAvailableDictionaryUsername()
  return { username }
}
