import { type ProviderId } from '../../auth/utils.js'
import { type PrismaClient } from '@prisma/client'

export type AuthHookFn =
  | OnUserSignUpHookFn
  | OnUserLoginHookFn
  | OnUserLogoutHookFn

type OnUserLoginHookFn = (context: {
  providerId: ProviderId
  prisma: PrismaClient
}) => Promise<void> | void
type OnUserSignUpHookFn = (context: {
  providerId: ProviderId
  prisma: PrismaClient
}) => Promise<void> | void
type OnUserLogoutHookFn = (context: {
  providerId: ProviderId
  prisma: PrismaClient
}) => Promise<void> | void

type HookType = 'onUserSignUp' | 'onUserLogin' | 'onUserLogout'

export function defineHook<HT extends HookType>(
  hookType: HT,
  hookFn: AuthHookFn,
): AuthHookFn {
  return hookFn
}
