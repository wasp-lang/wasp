import type {
  _Task,
  TaskDefinition,
} from '../_types'

// PUBLIC API
export type DeleteDoneTasks = TaskDefinition<[
  _Task,
]>

