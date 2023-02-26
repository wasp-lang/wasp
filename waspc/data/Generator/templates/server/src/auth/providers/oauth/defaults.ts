import { generateAvailableDictionaryUsername } from '../../../core/auth.js'

export async function getUserFieldsFn(_context, _args) {
  const username = await generateAvailableDictionaryUsername()
  return { username }
}
