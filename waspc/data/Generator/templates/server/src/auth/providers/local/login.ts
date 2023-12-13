{{={= =}=}}
import { verifyPassword, throwInvalidCredentialsError } from '../../../core/auth.js'
import { handleRejection } from '../../../utils.js'

import {
  findAuthIdentity,
  findAuthWithUserBy,
  createAuthToken,
  deserializeProviderData,
} from '../../utils.js'
import { ensureValidUsername, ensurePasswordIsPresent } from '../../validation.js'

export default handleRejection(async (req, res) => {
  const fields = req.body || {}
  ensureValidArgs(fields)

  const authIdentity = await findAuthIdentity('username', fields.username)
  if (!authIdentity) {
    console.log('authIdentity', authIdentity)
    throwInvalidCredentialsError()
  }

  try {
    const providerData = deserializeProviderData<'username'>(authIdentity.providerData)

    await verifyPassword(providerData.password, fields.password)
  } catch(e) {
    console.log('e', e)
    throwInvalidCredentialsError()
  }

  const auth = await findAuthWithUserBy({
    id: authIdentity.authId
  }) 
  const token = await createAuthToken(auth)

  return res.json({ token })
})

function ensureValidArgs(args: unknown): void {
  ensureValidUsername(args);
  ensurePasswordIsPresent(args);
}
