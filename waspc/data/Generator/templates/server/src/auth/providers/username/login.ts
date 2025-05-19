{{={= =}=}}
import { createInvalidCredentialsError } from 'wasp/auth/utils'
import { defineHandler } from 'wasp/server/utils'
import { verifyPassword } from 'wasp/auth/password'

import {
  createProviderId,
  findAuthIdentity,
  findAuthWithUserBy,
  getProviderDataWithPassword,
} from 'wasp/auth/utils'
import { createSession } from 'wasp/auth/session'
import { ensureValidUsername, ensurePasswordIsPresent } from 'wasp/auth/validation'
import { onBeforeLoginHook, onAfterLoginHook } from '../../hooks.js';

export default defineHandler(async (req, res) => {
  const fields = req.body ?? {}
  ensureValidArgs(fields)

  const providerId = createProviderId('username', fields.username)
  const authIdentity = await findAuthIdentity(providerId)
  if (!authIdentity) {
    throw createInvalidCredentialsError()
  }

  try {
    const providerData = getProviderDataWithPassword<'username'>(authIdentity.providerData)

    await verifyPassword(providerData.hashedPassword, fields.password)
  } catch(e) {
    throw createInvalidCredentialsError()
  }

  const auth = await findAuthWithUserBy({
    id: authIdentity.authId
  })

  if (auth === null) {
    throw createInvalidCredentialsError()
  }

  await onBeforeLoginHook({
      req,
      providerId,
      user: auth.user,
  })

  const session = await createSession(auth.id)

  await onAfterLoginHook({
    req,
    providerId,
    user: auth.user,
  })

  res.json({
      sessionId: session.id,
  })
})

function ensureValidArgs(args: object): void {
  ensureValidUsername(args);
  ensurePasswordIsPresent(args);
}
