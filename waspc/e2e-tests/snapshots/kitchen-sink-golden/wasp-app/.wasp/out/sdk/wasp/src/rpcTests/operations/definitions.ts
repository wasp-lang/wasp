import { Task } from "wasp/entities";
import type {
  BoolToStringNoAuth,
  GetAnythingAuth,
  GetAnythingNoAuth,
  GetAnyToNumberSpecified,
  GetDate,
  GetTrueVoid,
  TaskToTaskSatisfies,
  TaskToTaskSpecified,
  UnspecifiedToNumber,
  VoidToStringAuth,
  VoidToStringNoAuth,
} from "wasp/server/operations";

export const taskToTaskUnspecified = async (args: Task) => args;

export const taskToTaskSatisfies = (async (args: Task) =>
  args) satisfies TaskToTaskSatisfies;

export const taskToTaskSpecified: TaskToTaskSpecified<Task, Task> = async (
  args,
) => args;

export const voidToStringAuth: VoidToStringAuth<void, string> = async (
  args,
  context,
) => {
  console.log(
    "[voidToStringAuth] Received context (should have AuthUser with entities.Task): ",
    context,
  );
  console.log("[voidToStringAuth] Received args (should be undefined): ", args);
  return "foo";
};

export const voidToStringNoAuth: VoidToStringNoAuth<void, string> = async (
  args,
  context,
) => {
  console.log(
    "[voidToStringNoAuth] Received context (should only have entities.Task): ",
    context,
  );
  console.log(
    "[voidToStringNoAuth] Received args (should be undefined): ",
    args,
  );
  return "void";
};

export const unspecifiedToNumber = (async (_args) => {
  return 10;
}) satisfies UnspecifiedToNumber;

export const boolToStringNoAuth: BoolToStringNoAuth<boolean, string> = async (
  args,
  context,
) => {
  console.log(
    "[boolToStringNoAuth] Received context (should only have entities.Task): ",
    context,
  );
  console.log("[boolToStringNoAuth] Received args (should be boolean): ", args);
  return args.toString();
};

export const boolToStringAuth: BoolToStringNoAuth<boolean, string> = async (
  args,
  context,
) => {
  console.log(
    "[boolToStringAuth] Received context (should have AuthUser and entities.Task): ",
    context,
  );
  console.log("[boolToStringAuth] Received args (should be boolean): ", args);
  return args.toString();
};

export const boolToVoidNoAuth: BoolToStringNoAuth<boolean, void> = async (
  args,
  context,
) => {
  console.log(
    "[boolToVoidNoAuth] Received context (should only have entities.Task): ",
    context,
  );
  console.log("[boolToVoidNoAuth] Received args (should be boolean): ", args);
};

export const boolToVoidAuth: BoolToStringNoAuth<boolean, void> = async (
  args,
  context,
) => {
  console.log(
    "[boolToVoidAuth] Received context (should have AuthUser and entities.Task): ",
    context,
  );
  console.log("[boolToVoidAuth] Received args (should be boolean): ", args);
};

export const getDate: GetDate<void, Date> = async () => {
  return new Date();
};

export const getAnythingAuth: GetAnythingAuth = async () => {
  return "anything";
};

export const getAnythingNoAuth: GetAnythingNoAuth = async () => {
  return "anything";
};

export const getTrueVoid = (async () => {
  return "anything";
}) satisfies GetTrueVoid;

export const getAnyNoAuth = (_args: any, _context: any): any => {
  return "anything";
};

export const getAnyAuth = (_args: any, _context: any): any => {
  return "anything";
};

export const getAnyToNumberSpecified: GetAnyToNumberSpecified<any, number> = (
  _args,
  _context,
) => {
  return 10;
};
