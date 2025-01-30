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
  getAnythingAuth,
  getAnythingNoAuth,
  getTrueVoid,
  getAnyNoAuth,
  type TestingAction,
  getAnyAuth,
  getAnyToNumberSpecified,
  jsActionWithArgs,
} from 'wasp/server/operations'

import {
  taskToTaskUnspecified as taskToTaskUnspecifiedDefinition,
  taskToTaskSatisfies as taskToTaskSatisfiesDefinition,
} from './definitions'

import { Equal, Expect } from '../helpers'
import { AuthUser } from 'wasp/auth'
import { Task } from 'wasp/entities'
import { Payload } from 'wasp/server/_types'

export const testingAction: TestingAction = async (_args, context) => {
  // TODO: (Filip) When sorting out the tests, we should also test whether the
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
  Expect<
    Equal<
      typeof taskToTaskUnspecified,
      (
        args: Task,
        ctx: { user: AuthUser }
      ) => ReturnType<typeof taskToTaskUnspecifiedDefinition>
    >
  >,
  Expect<
    Equal<
      typeof taskToTaskSatisfies,
      (
        args: Task,
        ctx: { user: AuthUser }
      ) => ReturnType<typeof taskToTaskSatisfiesDefinition>
    >
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
  >,
  Expect<
    Equal<
      typeof getAnythingAuth,
      (args: unknown, ctx: { user: AuthUser }) => Promise<Payload>
    >
  >,
  Expect<Equal<typeof getAnythingNoAuth, (args?: unknown) => Promise<Payload>>>,
  Expect<
    Equal<typeof getTrueVoid, (ctx: { user: AuthUser }) => Promise<string>>
  >,
  Expect<Equal<typeof getAnyNoAuth, (args?: any) => Promise<any>>>,
  Expect<
    Equal<
      typeof getAnyAuth,
      (args: any, ctx: { user: AuthUser }) => Promise<any>
    >
  >,
  Expect<
    Equal<
      typeof getAnyToNumberSpecified,
      (args: any, ctx: { user: AuthUser }) => Promise<number>
    >
  >,
  // TODO: update this test to include a function that destructures
  // the `args` object. This currently fails with functions imported
  // from JavaScript files.
  Expect<
    Equal<
      typeof jsActionWithArgs,
      (args: any, ctx: { user: AuthUser }) => Promise<void>
    >
  >
]
