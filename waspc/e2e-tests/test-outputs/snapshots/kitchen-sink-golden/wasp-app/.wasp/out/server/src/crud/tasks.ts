import { prisma } from 'wasp/server'

import { createInvalidCredentialsError } from 'wasp/server/auth/utils'
import type {
  RegisteredGetAllQuery,
  RegisteredGetQuery,
  RegisteredCreateAction,
  RegisteredUpdateAction,
  RegisteredDeleteAction,
} from 'wasp/server/crud/tasks'
import { crudGetAllTasks } from '../../../../../src/features/crud/crud'
import { crudCreateTask } from '../../../../../src/features/crud/crud'

const entities = {
  Task: prisma.task,
}

// Get All query
const _waspGetAllQuery = crudGetAllTasks

export async function getAllFn(args, context) {
  return (_waspGetAllQuery as any)(args, {
    ...context,
    entities,
  });
}

// Get query
const _waspGetQuery: RegisteredGetQuery = ((args, context) => {
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
const _waspCreateAction = crudCreateTask

export async function createFn(args, context) {
  return (_waspCreateAction as any)(args, {
    ...context,
    entities,
  });
}

// Update action
const _waspUpdateAction: RegisteredUpdateAction = ((args, context) => {
  throwIfNotAuthenticated(context)
  const { id: idFieldValue, ...rest } = args
  return context.entities.Task.update({
    where: { id: idFieldValue },
    data: rest,
  });
});

export async function updateFn(args, context) {
  return (_waspUpdateAction as any)(args, {
    ...context,
    entities,
  });
}

// Delete action
const _waspDeleteAction: RegisteredDeleteAction = ((args, context) => {
  throwIfNotAuthenticated(context)
  const { id: idFieldValue } = args
  return context.entities.Task.delete({ where:  { id: idFieldValue } });
});

export async function deleteFn(args, context) {
  return (_waspDeleteAction as any)(args, {
    ...context,
    entities,
  });
}

function throwIfNotAuthenticated (context) {
  if (!context.user) {
    throw createInvalidCredentialsError()
  }
}
