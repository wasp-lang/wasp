import { prisma } from 'wasp/server'

import { createInvalidCredentialsError } from 'wasp/auth/utils'
import type {
  GetAllQueryResolved,
  GetQueryResolved,
  CreateActionResolved,
} from 'wasp/server/crud/tasks'

const entities = {
  Task: prisma.task,
}

// Get All query
const _waspGetAllQuery: GetAllQueryResolved = ((args, context) => {
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
const _waspGetQuery: GetQueryResolved = ((args, context) => {
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
const _waspCreateAction: CreateActionResolved = ((args, context) => {
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
    throw createInvalidCredentialsError()
  }
}