{{={= =}=}}
import { throwInvalidCredentialsError } from '../../utils.js'
import { handleRejection } from 'wasp/server/utils'
import { verifyPassword } from '../../password.js'

import {
  createProviderId,
  findAuthIdentity,
  findAuthWithUserBy,
  deserializeAndSanitizeProviderData,
} from '../../utils.js'
import { createSession } from '../../session.js'
import { ensureValidUsername, ensurePasswordIsPresent } from '../../validation.js'

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
