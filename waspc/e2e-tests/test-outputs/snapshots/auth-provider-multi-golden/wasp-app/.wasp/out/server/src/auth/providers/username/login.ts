import { createInvalidCredentialsError } from 'wasp/server/auth/utils'
import { defineHandler } from 'wasp/server/utils'
import { verifyPassword } from 'wasp/server/auth/password'

import { waspAuthRuntime } from 'wasp/server/auth/provider'
import { ensureValidUsername, ensurePasswordIsPresent } from 'wasp/auth/validation'

export default defineHandler(async (req, res) => {
  const fields = req.body ?? {}
  ensureValidArgs(fields)

  const usernameIdentities = waspAuthRuntime.identityNamespaces('username')
  const identity = await usernameIdentities.find(fields.username)
  if (!identity) {
    throw createInvalidCredentialsError()
  }

  try {
    // The secret column is read explicitly and only here in this flow.
    const secrets = await usernameIdentities.getSecrets(fields.username)
    if (secrets === null || typeof secrets.hashedPassword !== 'string') {
      throw createInvalidCredentialsError()
    }
    await verifyPassword(secrets.hashedPassword, fields.password)
  } catch(e) {
    throw createInvalidCredentialsError()
  }

  // The mint goes through the same `wasp-sessions` facet an adapter package
  // gets; the app's login hooks fire inside it (a throw vetoes the mint).
  const { sessionId } = await waspAuthRuntime.sessions.issue(
    { namespace: 'username', subjectId: fields.username },
    { req },
  )

  res.json({
      sessionId,
  })
})

function ensureValidArgs(args: object): void {
  ensureValidUsername(args);
  ensurePasswordIsPresent(args);
}
