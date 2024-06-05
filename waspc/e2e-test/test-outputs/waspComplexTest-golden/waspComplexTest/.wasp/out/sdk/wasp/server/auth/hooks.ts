import type { Request as ExpressRequest } from 'express'
import type { ProviderId, createUser } from '../../auth/utils.js'
import { prisma } from '../index.js'

type CommonInput = {
  hookName: string
  prisma: typeof prisma
  req: ExpressRequest
}

/* On Before Signup Hook */
export type OnBeforeSignupHookFn = (
  params: OnBeforeSignupHookFnInput,
) => Promise<void>

type OnBeforeSignupHookFnInput = {
  providerId: ProviderId
} & CommonInput

/* On After Signup Hook */
export type OnAfterSignupHookFn = (
  params: OnAfterSignupHookFnInput,
) => Promise<void>

type OnAfterSignupHookFnInput = {
  providerId: ProviderId
  user: Awaited<ReturnType<typeof createUser>>
  oauth?: {
    accessToken: string
    uniqueRequestId: string
  },
} & CommonInput

/* On Before OAuth Redirect Hook */
export type OnBeforeOAuthRedirectHookFn = (
  params: OnBeforeOAuthRedirectHookFnInput,
) => Promise<{ url: URL }>

type OnBeforeOAuthRedirectHookFnInput = {
  url: URL
  uniqueRequestId: string
} & CommonInput
