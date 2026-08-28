import { createInvalidCredentialsError } from 'wasp/server/auth/utils'
import { defineHandler } from 'wasp/server/utils'
import { verifyPassword } from 'wasp/server/auth/password'

import {
  createProviderId,
  findAuthWithUserBy,
} from 'wasp/server/auth/utils'
import { getIdentityStore } from 'wasp/server/auth/identityStore'
import { createSession } from 'wasp/server/auth/session'
import { ensureValidUsername, ensurePasswordIsPresent } from 'wasp/auth/validation'
import { onBeforeLoginHook, onAfterLoginHook } from '../../hooks.js';

export default defineHandler(async (req, res) => {
  const fields = req.body ?? {}
  ensureValidArgs(fields)

  const usernameIdentities = getIdentityStore('username')
  const providerId = createProviderId('username', fields.username)
  const identity = await usernameIdentities.find(fields.username)
  if (!identity) {
    throw createInvalidCredentialsError()
  }

  try {
    // The secret column is read explicitly and only here in this flow.
    const secrets = await usernameIdentities.getSecrets(fields.username)
    if (secrets === null) {
      throw createInvalidCredentialsError()
    }
    await verifyPassword(secrets.hashedPassword, fields.password)
  } catch(e) {
    throw createInvalidCredentialsError()
  }

  const auth = await findAuthWithUserBy({
    id: identity.authId
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
