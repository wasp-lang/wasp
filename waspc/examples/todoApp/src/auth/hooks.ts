import { Request } from 'express'
import type {
  OnAfterSignupHookFn,
  OnBeforeOAuthRedirectHookFn,
  OnBeforeSignupHookFn,
} from 'wasp/server/auth'

export const onBeforeSignup: OnBeforeSignupHookFn = async (args) => {
  const log = createLoggerForHook(args.hookName)
  const count = await args.prisma.user.count()
  log('number of users before', count)
  log('providerId object', args.providerId)
}

const oAuthQueryStore = new Map<string, Request['query']>()

export const onAfterSignup: OnAfterSignupHookFn = async (args) => {
  const log = createLoggerForHook(args.hookName)
  const count = await args.prisma.user.count()
  log('number of users after', count)
  log('user object', args.user)
  log('providerId object', args.providerId)

  // If this is a OAuth signup, we have access token and uniqueRequestId
  if (args.oauth) {
    log('accessToken', args.oauth.accessToken)
    log('uniqueRequestId', args.oauth.uniqueRequestId)
    const id = args.oauth.uniqueRequestId
    const query = oAuthQueryStore.get(id)
    if (query) {
      log('saved query params after oAuth redirect', query)
    }
    oAuthQueryStore.delete(id)
  }
}

export const onBeforeOAuthRedirect: OnBeforeOAuthRedirectHookFn = async (
  args
) => {
  const log = createLoggerForHook(args.hookName)
  log('query params before oAuth redirect', args.req.query)

  // Saving query params for later use in onAfterSignup hook
  const id = args.uniqueRequestId
  oAuthQueryStore.set(id, args.req.query)

  return { url: args.url }
}

function createLoggerForHook(hookName: string) {
  return (...args: unknown[]) => {
    console.log(`[${hookName}]`, ...args)
  }
}
