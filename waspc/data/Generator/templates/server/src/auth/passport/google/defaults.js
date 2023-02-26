import { generateAvailableDictionaryUsername } from '../../../../core/auth.js'

// Default implementation if there is no `auth.methods.google.configFn`.
export function configFn() {
  return {}
}

// Default implementation if there is no `auth.methods.google.getUserFieldsFn`.
export async function getUserFieldsFn(_context, _args) {
  const username = await generateAvailableDictionaryUsername()
  return { username }
}
