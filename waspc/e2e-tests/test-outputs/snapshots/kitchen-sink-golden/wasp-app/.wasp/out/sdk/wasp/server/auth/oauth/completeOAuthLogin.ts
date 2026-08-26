import { type Response } from 'express'
import { getOAuthLoginResultUrl } from './redirect.js'
import { createSession } from '../session.js'
import { appDelivery } from '../../core/delivery.js'

// PUBLIC API
export async function completeOAuthLogin({
  authId,
  response,
}: {
  authId: string
  response: Response
}): Promise<URL> {
  const session = await createSession(authId)
  appDelivery.respondWithSession(response, session.id)
  return getOAuthLoginResultUrl()
}
