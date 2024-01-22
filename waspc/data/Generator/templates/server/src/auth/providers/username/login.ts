{{={= =}=}}
import { throwInvalidCredentialsError } from 'wasp/auth/utils'
import { handleRejection } from 'wasp/server/utils'
import { verifyPassword } from 'wasp/auth/password'

import {
  createProviderId,
  findAuthIdentity,
  findAuthWithUserBy,
  deserializeAndSanitizeProviderData,
} from 'wasp/auth/utils'
import { createSession } from 'wasp/auth/session'
import { ensureValidUsername, ensurePasswordIsPresent } from 'wasp/auth/validation'

export default handleRejection(async (req, res) => {
  const fields = req.body ?? {}
  ensureValidArgs(fields)

  const providerId = createProviderId('username', fields.username)
  const authIdentity = await findAuthIdentity(providerId)
  if (!authIdentity) {
    throwInvalidCredentialsError()
  }

  try {
    const providerData = deserializeAndSanitizeProviderData<'username'>(authIdentity.providerData)

    await verifyPassword(providerData.hashedPassword, fields.password)
  } catch(e) {
    throwInvalidCredentialsError()
  }

  const auth = await findAuthWithUserBy({
    id: authIdentity.authId
  }) 

  const session = await createSession(auth.id)

  return res.json({
      sessionId: session.id,
  })
})

function ensureValidArgs(args: unknown): void {
  ensureValidUsername(args);
  ensurePasswordIsPresent(args);
}
