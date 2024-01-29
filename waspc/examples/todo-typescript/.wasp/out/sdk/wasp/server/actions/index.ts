import { prisma } from 'wasp/server'

import { createTask as createTask_ext } from 'wasp/ext-src/task/actions.js'
import { updateTask as updateTask_ext } from 'wasp/ext-src/task/actions.js'
import { deleteTasks as deleteTasks_ext } from 'wasp/ext-src/task/actions.js'
import { send as send_ext } from 'wasp/ext-src/user/customEmailSending.js'

export type CreateTask = typeof createTask_ext 

export const createTask = async (args, context) => {
  return (createTask_ext as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}

export type UpdateTask = typeof updateTask_ext 

export const updateTask = async (args, context) => {
  return (updateTask_ext as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}

export type DeleteTasks = typeof deleteTasks_ext 

export const deleteTasks = async (args, context) => {
  return (deleteTasks_ext as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}

export type CustomEmailSending = typeof send_ext 

export const customEmailSending = async (args, context) => {
  return (send_ext as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}
