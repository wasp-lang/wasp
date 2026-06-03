import { action, query, type Decl } from "@wasp.sh/spec";

import {
  boolToStringAuth,
  boolToStringNoAuth,
  boolToVoidAuth,
  boolToVoidNoAuth,
  getAnyAuth,
  getAnyNoAuth,
  getAnythingAuth,
  getAnythingNoAuth,
  getAnyToNumberSpecified,
  getDate,
  getTrueVoid,
  taskToTaskSatisfies,
  taskToTaskSpecified,
  taskToTaskUnspecified,
  unspecifiedToNumber,
  voidToStringAuth,
  voidToStringNoAuth,
} from "./operations/definitions" with { type: "ref" };
import { jsActionWithArgs } from "./operations/jsDefinitions" with { type: "ref" };
import { testingAction } from "./operations/server" with { type: "ref" };

export const rpcTestsDecls: Decl[] = [
  action(testingAction, { entities: [] }),
  query(getDate),
  query(getAnythingNoAuth, { auth: false, entities: [] }),
  query(getAnythingAuth, { auth: true, entities: [] }),
  query(getTrueVoid, { entities: [] }),
  query(getAnyNoAuth, { auth: false, entities: [] }),
  query(getAnyAuth, { auth: true, entities: [] }),
  query(getAnyToNumberSpecified, { auth: true, entities: [] }),
  action(taskToTaskUnspecified, { entities: ["Task"] }),
  action(taskToTaskSatisfies, { entities: ["Task"] }),
  action(taskToTaskSpecified, { entities: ["Task"] }),
  action(voidToStringAuth, { auth: true, entities: ["Task"] }),
  action(voidToStringNoAuth, { auth: false, entities: ["Task"] }),
  action(unspecifiedToNumber, { entities: ["Task"] }),
  action(boolToStringAuth, { auth: true, entities: ["Task"] }),
  action(boolToStringNoAuth, { auth: false, entities: ["Task"] }),
  action(boolToVoidNoAuth, { auth: false, entities: ["Task"] }),
  action(boolToVoidAuth, { auth: true, entities: ["Task"] }),
  action(jsActionWithArgs, { auth: true, entities: ["Task"] }),
];
