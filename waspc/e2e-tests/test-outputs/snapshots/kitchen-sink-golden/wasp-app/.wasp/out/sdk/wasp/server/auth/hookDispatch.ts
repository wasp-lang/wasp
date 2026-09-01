import { prisma } from '../index.js'
import type {
  OnAfterLoginHook,
  OnAfterSignupHook,
  OnBeforeLoginHook,
  OnBeforeSignupHook,
} from './hooks.js'
import { onBeforeSignup as onBeforeSignupHook_ext } from 'virtual:wasp/user/features/auth/hooks'
import { onAfterSignup as onAfterSignupHook_ext } from 'virtual:wasp/user/features/auth/hooks'
import { onBeforeLogin as onBeforeLoginHook_ext } from 'virtual:wasp/user/features/auth/hooks'
import { onAfterLogin as onAfterLoginHook_ext } from 'virtual:wasp/user/features/auth/hooks'

/**
 * PRIVATE API. Dispatch for the app-level auth lifecycle hooks
 * (`auth.hooks` in main.wasp.ts).
 *
 * These fire at Wasp-owned choke points -- identity provisioning and session
 * minting -- so they cover EVERY provider: Wasp's own methods, adapter
 * packages, hand-written providers. A provider can neither forget nor forge
 * them, because they run where Wasp owns the control flow.
 *
 * Each wrapper injects `prisma` so user hooks can touch the database without
 * importing anything.
 */

type InternalFunctionForHook<HookFn extends (params: never) => unknown> = (
  params: Omit<Parameters<HookFn>[0], 'prisma'>,
) => ReturnType<HookFn>

export const onBeforeSignupHook: InternalFunctionForHook<OnBeforeSignupHook> = (params) =>
  onBeforeSignupHook_ext({ prisma, ...params })

export const onAfterSignupHook: InternalFunctionForHook<OnAfterSignupHook> = (params) =>
  onAfterSignupHook_ext({ prisma, ...params })

export const onBeforeLoginHook: InternalFunctionForHook<OnBeforeLoginHook> = (params) =>
  onBeforeLoginHook_ext({ prisma, ...params })

export const onAfterLoginHook: InternalFunctionForHook<OnAfterLoginHook> = (params) =>
  onAfterLoginHook_ext({ prisma, ...params })
