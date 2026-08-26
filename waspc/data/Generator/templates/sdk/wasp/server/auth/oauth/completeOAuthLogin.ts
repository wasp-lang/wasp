{{={= =}=}}
import { type Response } from 'express'
{=# usesSessionHandoff =}
import {
  getOAuthLoginResultUrlWithSessionHandoffCode,
} from './redirect.js'
import { sessionHandoffCodes } from './sessionHandoff.js'
{=/ usesSessionHandoff =}
{=^ usesSessionHandoff =}
import { getOAuthLoginResultUrl } from './redirect.js'
import { createSession } from '../session.js'
import { appDelivery } from '../../core/delivery.js'
{=/ usesSessionHandoff =}

// PUBLIC API
export async function completeOAuthLogin({
  authId,
  response,
}: {
  authId: string
  response: Response
}): Promise<URL> {
  {=# usesSessionHandoff =}
  const sessionHandoffCode = await sessionHandoffCodes.issue(authId)
  return getOAuthLoginResultUrlWithSessionHandoffCode(sessionHandoffCode)
  {=/ usesSessionHandoff =}
  {=^ usesSessionHandoff =}
  const session = await createSession(authId)
  appDelivery.respondWithSession(response, session.id)
  return getOAuthLoginResultUrl()
  {=/ usesSessionHandoff =}
}
