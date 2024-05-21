import { prisma } from 'wasp/server'
import type {
  TestingAction,
  VoidToNumberAuth,
  VoidToNumberNoAuth,
  BoolToNumberNoAuth,
} from 'wasp/server/operations'
import {
  voidToNumberAuth as waspVoidToNumberAuth,
  voidToNumberNoAuth as waspVoidToNumberNoAuth,
  boolToNumberNoAuth as waspBoolToNumberNoAuth,
  boolToNumberAuth as waspBoolToNumberAuth,
} from 'wasp/server/operations'

export const testingAction: TestingAction = async (args, context) => {
  const result1 = await waspVoidToNumberNoAuth()
  const result2 = await waspBoolToNumberNoAuth(true)

  const user = context.user!

  const result3 = await waspVoidToNumberAuth({ user })
  const result4 = await waspBoolToNumberAuth(true, { user })
  // todo test when function "returns" void
}

export const voidToNumberAuth: VoidToNumberAuth<void, number> = async (
  args,
  context
) => {
  console.log(
    '[voidToNumberAuth] Received context (should be AuthUser with Task): ',
    context
  )
  console.log('[voidToNumberAuth] Received args (should be undefined): ', args)
  const user = context.user!
  const numberOfTasks = await context.entities.Task.count({
    where: { user: { id: user.id } },
  })
  console.log(
    `[voidToNumberAuth] Number of tasks of user ${user.id}:`,
    numberOfTasks
  )
  return numberOfTasks
}

export const voidToNumberNoAuth: VoidToNumberNoAuth<void, number> = async (
  args,
  context
) => {
  console.log(
    '[voidToNumberNoAuth] Received context (should only have entities.Task): ',
    context
  )
  console.log(
    '[voidToNumberNoAuth] Received args (should be undefined): ',
    args
  )
  const numberOfTasks = await context.entities.Task.count()
  return numberOfTasks
}

export const boolToNumberNoAuth: BoolToNumberNoAuth<boolean, number> = async (
  args,
  context
) => {
  console.log(
    '[boolToNumberNoAuth] Received context (should only have entities.Task): ',
    context
  )
  console.log('[boolToNumberNoAuth] Received args (should be boolean): ', args)
  return context.entities.Task.count()
}

export const boolToNumberAuth: BoolToNumberNoAuth<boolean, number> = async (
  args,
  context
) => {
  console.log(
    '[boolToNumberAuth] Received context (should only have entities.Task): ',
    context
  )
  console.log('[boolToNumberAuth] Received args (should be boolean): ', args)
  return context.entities.Task.count()
}