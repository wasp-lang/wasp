import { prisma } from 'wasp/server';

import type { Prisma } from "@prisma/client";
import type {
  Task,
} from "wasp/entities";
import { throwInvalidCredentialsError } from 'wasp/auth/utils'
import type { tasks } from "wasp/server/crud";

type _WaspEntity = Task
const entities = {
  Task: prisma.task,
}

// Get All query
type GetAllInput = {}
type GetAllOutput = _WaspEntity[]
const _waspGetAllQuery: tasks.GetAllQuery<GetAllInput, GetAllOutput> = ((args, context) => {
  throwIfNotAuthenticated(context)
  return context.entities.Task.findMany();
});

export async function getAllFn(args, context) {
  return (_waspGetAllQuery as any)(args, {
    ...context,
    entities,
  });
}

// Get query
type GetInput = Prisma.TaskWhereUniqueInput
type GetOutput = _WaspEntity | null
const _waspGetQuery: tasks.GetQuery<GetInput, GetOutput> = ((args, context) => {
  throwIfNotAuthenticated(context)
  return context.entities.Task.findUnique({ where: { id: args.id } });
});

export async function getFn(args, context) {
  return (_waspGetQuery as any)(args, {
    ...context,
    entities,
  });
}

// Create action
type CreateInput = Prisma.TaskCreateInput
type CreateOutput = _WaspEntity
const _waspCreateAction: tasks.CreateAction<CreateInput, CreateOutput> = ((args, context) => {
  throwIfNotAuthenticated(context)
  return context.entities.Task.create({ data: args });
});

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