import { prisma } from 'wasp/server'

import { createInvalidCredentialsError } from 'wasp/server/auth/utils'
import type {
  RegisteredGetAllQuery,
} from 'wasp/server/crud/taskVotes'

const entities = {
  TaskVote: prisma.taskVote,
}

// Get All query
const _waspGetAllQuery: RegisteredGetAllQuery = ((args, context) => {
  throwIfNotAuthenticated(context)
  return context.entities.TaskVote.findMany();
});

export async function getAllFn(args, context) {
  return (_waspGetAllQuery as any)(args, {
    ...context,
    entities,
  });
}

function throwIfNotAuthenticated (context) {
  if (!context.user) {
    throw createInvalidCredentialsError()
  }
}
