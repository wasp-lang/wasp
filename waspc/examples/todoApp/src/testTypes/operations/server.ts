import {
  voidToStringAuth,
  voidToStringNoAuth,
  boolToStringNoAuth,
  boolToStringAuth,
  boolToVoidNoAuth,
  boolToVoidAuth,
  unspecifiedToNumber,
  taskToTaskUnspecified,
  taskToTaskSatisfies,
  taskToTaskSpecified,
  type TestingAction,
} from 'wasp/server/operations'

import {
  taskToTaskUnspecified as taskToTaskUnspecifiedDefinition,
  taskToTaskSatisfies as taskToTaskSatisfiesDefinition,
} from './definitions'

import { Equal, Expect } from '../helpers'
import { AuthUser } from 'wasp/auth'
import { Task } from 'wasp/entities'

export const testingAction: TestingAction = async (args, context) => {
  // todo(filip): When sorting out the tests, we should also test whether the
  // outputs are correct. See:
  // - https://github.com/wasp-lang/wasp/issues/2024
  // - https://github.com/wasp-lang/wasp/issues/2011
  await voidToStringNoAuth()
  await boolToStringNoAuth(true)

  const user = context.user
  if (!user) {
    return
  }

  await voidToStringAuth({ user })
  await boolToStringAuth(true, { user })

  await boolToVoidNoAuth(true)
  await boolToVoidAuth(true, { user })
}

type TestCases = [
  // todo(filip): Prisma errors casuing this test to fail, try to add Except
  // after updating Prisma: https://github.com/wasp-lang/wasp/issues/2099
  Equal<
    typeof taskToTaskUnspecified,
    (
      args: Task,
      ctx: { user: AuthUser }
    ) => ReturnType<typeof taskToTaskUnspecifiedDefinition>
  >,
  // todo(filip): Prisma errors casuing this test to fail, try to add Except
  // after updating Prisma: https://github.com/wasp-lang/wasp/issues/2099
  Equal<
    typeof taskToTaskSatisfies,
    (
      args: Task,
      ctx: { user: AuthUser }
    ) => ReturnType<typeof taskToTaskSatisfiesDefinition>
  >,
  Expect<
    Equal<
      typeof taskToTaskSpecified,
      (args: Task, ctx: { user: AuthUser }) => Promise<Task>
    >
  >,
  Expect<
    Equal<
      typeof unspecifiedToNumber,
      (args: unknown, ctx: { user: AuthUser }) => Promise<number>
    >
  >,
  Expect<Equal<typeof voidToStringNoAuth, () => Promise<string>>>,
  Expect<
    Equal<typeof boolToStringNoAuth, (payload: boolean) => Promise<string>>
  >,
  Expect<
    Equal<typeof voidToStringAuth, (ctx: { user: AuthUser }) => Promise<string>>
  >,
  Expect<
    Equal<
      typeof boolToStringAuth,
      (payload: boolean, ctx: { user: AuthUser }) => Promise<string>
    >
  >,
  Expect<Equal<typeof boolToVoidNoAuth, (payload: boolean) => Promise<void>>>,
  Expect<
    Equal<
      typeof boolToVoidAuth,
      (payload: boolean, ctx: { user: AuthUser }) => Promise<void>
    >
  >
]
