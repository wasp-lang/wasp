import { AuthUser } from 'wasp/auth'
import { getMe } from 'wasp/client/auth'
import {
  getTask,
  getTasks,
  createTask,
  updateTaskIsDone,
  deleteCompletedTasks,
  toggleAllTasks,
  getNumTasks,
  getDate,
  getAnything,
  getTrueVoid,
} from 'wasp/client/operations'
import { Task } from 'wasp/entities'
import { Payload } from 'wasp/server/_types'
import { Assert, AreEqual } from './helpers'

// For the details of this specification, see
// https://github.com/wasp-lang/wasp/pull/1090#discussion_r1159732471
// This should be [],
// but I couldn't get it to work yet: https://github.com/wasp-lang/wasp/issues/2004
type VoidOperationPayload = [args?: void | undefined]

// When the user doesn't specify an operation payload,
// we want to be as permissive as possible.
type UnspecifiedOperationPayload = [args?: unknown]

type TQ1 = Assert<
  InputsAndOutputsAre<typeof getTask, [Pick<Task, 'id'>], Promise<Task>>
>

type TQ2 = Assert<
  InputsAndOutputsAre<typeof getTasks, VoidOperationPayload, Promise<Task[]>>
>

type TQ3 = Assert<
  InputsAndOutputsAre<typeof getNumTasks, VoidOperationPayload, Promise<number>>
>

type TQ4 = Assert<
  InputsAndOutputsAre<typeof getDate, VoidOperationPayload, Promise<Date>>
>

type TQ5 = Assert<
  InputsAndOutputsAre<
    typeof getAnything,
    UnspecifiedOperationPayload,
    Promise<Payload>
  >
>

type TQ6 = Assert<
  InputsAndOutputsAre<typeof getTrueVoid, VoidOperationPayload, Promise<string>>
>

type TestGetMe = Assert<
  InputsAndOutputsAre<
    typeof getMe,
    VoidOperationPayload,
    Promise<AuthUser | null>
  >
>

type TA1 = Assert<
  InputsAndOutputsAre<
    typeof createTask,
    [Pick<Task, 'description'>],
    Promise<Task>
  >
>

type TA2 = Assert<
  InputsAndOutputsAre<
    typeof updateTaskIsDone,
    [Pick<Task, 'id' | 'isDone'>],
    Promise<Payload>
  >
>

type TA3 = Assert<
  InputsAndOutputsAre<
    typeof deleteCompletedTasks,
    UnspecifiedOperationPayload,
    Promise<Payload>
  >
>

type TA4 = Assert<
  InputsAndOutputsAre<
    typeof toggleAllTasks,
    UnspecifiedOperationPayload,
    Promise<Payload>
  >
>

type InputsAndOutputsAre<
  OperationType extends (...args: any[]) => any,
  ExpectedParams,
  ExpectedReturn
> = {
  params: AreEqual<Parameters<OperationType>, ExpectedParams>
  return: AreEqual<ReturnType<OperationType>, ExpectedReturn>
}
