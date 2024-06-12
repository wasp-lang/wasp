import { prisma } from 'wasp/server'
import type {
  TestingAction,
  VoidToStringAuth,
  VoidToStringNoAuth,
  BoolToStringNoAuth,
} from 'wasp/server/operations'
import {
  voidToStringAuth as waspVoidToStringAuth,
  voidToStringNoAuth as waspVoidToStringNoAuth,
  boolToStringNoAuth as waspBoolToStringNoAuth,
  boolToStringAuth as waspBoolToStringAuth,
} from 'wasp/server/operations'

export const testingAction: TestingAction = async (args, context) => {
  const result1 = await waspVoidToStringNoAuth()
  const result2 = await waspBoolToStringNoAuth(true)

  const user = context.user!

  const result3 = await waspVoidToStringAuth({ user })
  const result4 = await waspBoolToStringAuth(true, { user })
  // todo test when function "returns" void
}

export const voidToStringAuth: VoidToStringAuth<void, number> = async (
  args,
  context
) => {
  console.log(
    '[voidToStringAuth] Received context (should be AuthUser with Task): ',
    context
  )
  console.log('[voidToStringAuth] Received args (should be undefined): ', args)
  const user = context.user!
  const numberOfTasks = await context.entities.Task.count({
    where: { user: { id: user.id } },
  })
  console.log(
    `[voidToStringAuth] String of tasks of user ${user.id}:`,
    numberOfTasks
  )
  return numberOfTasks
}

export const voidToStringNoAuth: VoidToStringNoAuth<void, number> = async (
  args,
  context
) => {
  console.log(
    '[voidToStringNoAuth] Received context (should only have entities.Task): ',
    context
  )
  console.log(
    '[voidToStringNoAuth] Received args (should be undefined): ',
    args
  )
  const numberOfTasks = await context.entities.Task.count()
  return numberOfTasks
}

export const boolToStringNoAuth: BoolToStringNoAuth<boolean, number> = async (
  args,
  context
) => {
  console.log(
    '[boolToStringNoAuth] Received context (should only have entities.Task): ',
    context
  )
  console.log('[boolToStringNoAuth] Received args (should be boolean): ', args)
  return context.entities.Task.count()
}

export const boolToStringAuth: BoolToStringNoAuth<boolean, number> = async (
  args,
  context
) => {
  console.log(
    '[boolToStringAuth] Received context (should only have entities.Task): ',
    context
  )
  console.log('[boolToStringAuth] Received args (should be boolean): ', args)
  return context.entities.Task.count()
}
