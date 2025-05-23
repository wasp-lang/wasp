import { AuthUser } from "wasp/auth";
import { getMe } from "wasp/client/auth";
import {
  boolToStringAuth,
  boolToStringNoAuth,
  boolToVoidAuth,
  boolToVoidNoAuth,
  getAnyAuth,
  getAnyNoAuth,
  getAnyToNumberSpecified,
  getAnythingAuth,
  getAnythingNoAuth,
  getDate,
  getSerializedObjects,
  getTrueVoid,
  jsActionWithArgs,
  taskToTaskSatisfies,
  taskToTaskSpecified,
  taskToTaskUnspecified,
  unspecifiedToNumber,
  voidToStringAuth,
  voidToStringNoAuth,
} from "wasp/client/operations";

import {
  taskToTaskSatisfies as taskToTaskSatisfiesDefinition,
  taskToTaskUnspecified as taskToTaskUnspecifiedDefinition,
} from "./definitions";

import { SERIALIZABLE_OBJECTS_FIXTURE } from "./fixtures";

import { QueryMetadata } from "wasp/client/operations/rpc";
import { Task } from "wasp/entities";
import { Payload } from "wasp/server/_types";
import { Equal, Expect } from "../helpers";

type TestCases = [
  Expect<Equal<typeof taskToTaskSpecified, (args: Task) => Promise<Task>>>,
  Expect<
    Equal<
      typeof taskToTaskUnspecified,
      (args: Task) => ReturnType<typeof taskToTaskUnspecifiedDefinition>
    >
  >,
  Expect<
    Equal<
      typeof taskToTaskSatisfies,
      (args: Task) => ReturnType<typeof taskToTaskSatisfiesDefinition>
    >
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
      typeof getSerializedObjects,
      QueryMetadata & (() => Promise<typeof SERIALIZABLE_OBJECTS_FIXTURE>)
    >
  >,
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
  Expect<
    Equal<typeof getAnyNoAuth, QueryMetadata & ((args?: any) => Promise<any>)>
  >,
  Expect<
    Equal<typeof getAnyAuth, QueryMetadata & ((args?: any) => Promise<any>)>
  >,
  Expect<
    Equal<
      typeof getAnyToNumberSpecified,
      QueryMetadata & ((args?: any) => Promise<number>)
    >
  >,
  Expect<Equal<typeof getMe, QueryMetadata & (() => Promise<AuthUser | null>)>>,
  Expect<Equal<typeof jsActionWithArgs, (args?: any) => Promise<void>>>,
];
