import { prisma } from '../index.js'
import type {
  OnAfterLoginHook,
  OnAfterSignupHook,
  OnBeforeLoginHook,
  OnBeforeSignupHook,
} from './hooks.js'

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

/**
 * Runs a veto-able hook (onBeforeSignup, onBeforeLogin) and tags whatever it
 * throws with the contract's `wasp-auth/policy-veto` code -- tagging, not
 * wrapping, so the error's type, message and any `statusCode` survive for
 * Wasp's own error handling, while an adapter package (which only speaks
 * contract codes) can map the rejection to a 4xx instead of a 500. An error
 * that already carries a code keeps it.
 */
export async function fireVetoableHook(fire: () => unknown): Promise<void> {
  try {
    await fire()
  } catch (error) {
    if (
      typeof error === 'object' &&
      error !== null &&
      !(error as { code?: unknown }).code
    ) {
      try {
        ;(error as { code?: string }).code = 'wasp-auth/policy-veto'
      } catch {
        // A frozen error object stays untagged; it still propagates.
      }
    }
    throw error
  }
}

export const onBeforeSignupHook: InternalFunctionForHook<OnBeforeSignupHook> = async (_params) => {}

export const onAfterSignupHook: InternalFunctionForHook<OnAfterSignupHook> = async (_params) => {}

export const onBeforeLoginHook: InternalFunctionForHook<OnBeforeLoginHook> = async (_params) => {}

export const onAfterLoginHook: InternalFunctionForHook<OnAfterLoginHook> = async (_params) => {}
