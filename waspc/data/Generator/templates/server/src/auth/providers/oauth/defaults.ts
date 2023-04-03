{{={= =}=}}
import { generateAvailableDictionaryUsername } from '../../../core/auth.js'

export async function getUserFieldsFn(_context, _args) {
  {=# isUsernameAndPasswordAuthEnabled =}
  const username = await generateAvailableDictionaryUsername()
  return { username }
  {=/ isUsernameAndPasswordAuthEnabled =}
  {=^ isUsernameAndPasswordAuthEnabled =}
  return {}
  {=/ isUsernameAndPasswordAuthEnabled =}
}
