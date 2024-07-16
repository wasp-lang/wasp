import { AuthUser } from 'wasp/auth'
import { getMe } from 'wasp/client/auth'
import {
  getDate,
  getAnythingAuth,
  getTrueVoid,
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
  getAnyAuth,
  getAnyNoAuth,
  getAnyToNumberSpecified,
  getAnythingNoAuth,
} from 'wasp/client/operations'

import {
  taskToTaskUnspecified as taskToTaskUnspecifiedDefinition,
  taskToTaskSatisfies as taskToTaskSatisfiesDefinition,
} from './definitions'

import { Task } from 'wasp/entities'
import { Payload } from 'wasp/server/_types'
import { Expect, Equal } from '../helpers'
import { QueryMetadata } from 'wasp/client/operations/rpc'

type TestCases = [
  Expect<Equal<typeof taskToTaskSpecified, (args: Task) => Promise<Task>>>,
  // todo(filip): Prisma errors casuing this test to fail, try to add Except
  // after updating Prisma: https://github.com/wasp-lang/wasp/issues/2099
  Equal<
    typeof taskToTaskUnspecified,
    (args: Task) => ReturnType<typeof taskToTaskUnspecifiedDefinition>
  >,
  // todo(filip): Prisma errors casuing this test to fail, try to add Except
  // after updating Prisma: https://github.com/wasp-lang/wasp/issues/2099
  Equal<
    typeof taskToTaskSatisfies,
    (args: Task) => ReturnType<typeof taskToTaskSatisfiesDefinition>
  >,
  Expect<
    Equal<typeof unspecifiedToNumber, (args?: unknown) => Promise<number>>
  >,
  Expect<Equal<typeof voidToStringNoAuth, () => Promise<string>>>,
  Expect<
    Equal<typeof boolToStringNoAuth, (payload: boolean) => Promise<string>>
  >,
  Expect<Equal<typeof voidToStringAuth, () => Promise<string>>>,
  Expect<Equal<typeof boolToStringAuth, (payload: boolean) => Promise<string>>>,
  Expect<Equal<typeof boolToVoidNoAuth, (payload: boolean) => Promise<void>>>,
  Expect<Equal<typeof boolToVoidAuth, (payload: boolean) => Promise<void>>>,
  Expect<Equal<typeof getDate, QueryMetadata & (() => Promise<Date>)>>,
  Expect<
    Equal<
      typeof getAnythingAuth,
      QueryMetadata & ((args?: unknown) => Promise<Payload>)
    >
  >,
  Expect<
    Equal<
      typeof getAnythingNoAuth,
      QueryMetadata & ((args?: unknown) => Promise<Payload>)
    >
  >,
  Expect<Equal<typeof getTrueVoid, QueryMetadata & (() => Promise<string>)>>,
  Expect<Equal<typeof getAnyNoAuth, QueryMetadata & ((args?: any) => Promise<any>)>>,
  Expect<Equal<typeof getAnyAuth, QueryMetadata & ((args?: any) => Promise<any>)>>,
  Expect<Equal<typeof getAnyToNumberSpecified, QueryMetadata & ((args?: any) => Promise<number>)>>,
  Expect<Equal<typeof getMe, QueryMetadata & (() => Promise<AuthUser | null>)>>,
]
