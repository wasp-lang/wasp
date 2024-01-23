import { createAction } from './core'
import { CreateTask } from 'wasp/server/actions'
import { UpdateTask } from 'wasp/server/actions'
import { DeleteTasks } from 'wasp/server/actions'

export const createTask = createAction<CreateTask>(
  'operations/create-task',
  ['Task'],
)

export const updateTask = createAction<UpdateTask>(
  'operations/update-task',
  ['Task'],
)

export const deleteTasks = createAction<DeleteTasks>(
  'operations/delete-tasks',
  ['Task'],
)
