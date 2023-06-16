import prisma from "../dbClient.js";

import type {
  AuthenticatedAction,
  AuthenticatedQuery,
  _Task,
} from "../_types";
import type {
  Prisma,
} from "@prisma/client";
import { Payload } from "../_types/serialization.js";
import type {
  Task,
} from "../entities";
import { throwInvalidCredentialsError } from "../core/auth.js";

type _WaspEntityTagged = _Task
type _WaspEntity = Task
const entities = {
  Task: prisma.task,
}

// Get All query
export type GetAllQuery<Input extends Payload, Output extends Payload> = AuthenticatedQuery<[_WaspEntityTagged], Input, Output>
type GetAllInput = {}
type GetAllOutput = _WaspEntity[]
const _waspGetAllQuery: GetAllQuery<GetAllInput, GetAllOutput> = ((args, context) => {
  throwIfNotAuthenticated(context)
  return context.entities.Task.findMany();
});

export type GetAllQueryResolved = typeof _waspGetAllQuery

export async function getAllFn(args, context) {
  return (_waspGetAllQuery as any)(args, {
    ...context,
    entities,
  });
}

// Get query
export type GetQuery<Input extends Payload, Output extends Payload> = AuthenticatedQuery<[_WaspEntityTagged], Input, Output>
type GetInput = Prisma.TaskWhereUniqueInput
type GetOutput = _WaspEntity | null
const _waspGetQuery: GetQuery<GetInput, GetOutput> = ((args, context) => {
  throwIfNotAuthenticated(context)
  return context.entities.Task.findUnique({ where: { id: args.id } });
});
export type GetQueryResolved = typeof _waspGetQuery

export async function getFn(args, context) {
  return (_waspGetQuery as any)(args, {
    ...context,
    entities,
  });
}

// Create action
export type CreateAction<Input extends Payload, Output extends Payload>= AuthenticatedAction<[_WaspEntityTagged], Input, Output>
type CreateInput = Prisma.TaskCreateInput
type CreateOutput = _WaspEntity
const _waspCreateAction: CreateAction<CreateInput, CreateOutput> = ((args, context) => {
  throwIfNotAuthenticated(context)
  return context.entities.Task.create({ data: args });
});

export type CreateActionResolved = typeof _waspCreateAction

export async function createFn(args, context) {
  return (_waspCreateAction as any)(args, {
    ...context,
    entities,
  });
}



function throwIfNotAuthenticated (context) {
  if (!context.user) {
    throwInvalidCredentialsError()
  }
}
