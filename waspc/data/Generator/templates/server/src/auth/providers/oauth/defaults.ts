{{={= =}=}}
import { generateAvailableDictionaryUsername } from '../../../core/auth.js'

export async function getUserFieldsFn(_context, _args) {
  {=# isUsernameOnUserEntity =}
  const username = await generateAvailableDictionaryUsername()
  return { username }
  {=/ isUsernameOnUserEntity =}
  {=^ isUsernameOnUserEntity =}
  return {}
  {=/ isUsernameOnUserEntity =}
}
