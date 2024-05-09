import { Link } from 'wasp/client/router'
import { type Task } from 'wasp/entities'

import {
  useAction,
  type OptimisticUpdateDefinition,
  updateTaskIsDone,
  useQuery,
  getTask,
  getTasks,
} from 'wasp/client/operations'

import React from 'react'

type TaskPayload = Pick<Task, 'id' | 'isDone'>

const Todo = (props: any) => {
  const taskId = parseInt(props.match.params.id)

  const {
    data: task,
    isFetching,
    error,
    isError,
  } = useQuery(getTask, { id: taskId })

  const updateTaskIsDoneOptimistically = useAction(updateTaskIsDone, {
    optimisticUpdates: [
      {
        getQuerySpecifier: () => [getTask, { id: taskId }],
        // Since we're on a page that uses this Query, its query's cache should
        // should never be empty.
        updateQuery: ({ isDone }, oldTask) => ({ ...oldTask!, isDone }),
      } as OptimisticUpdateDefinition<TaskPayload, Task>,
      {
        getQuerySpecifier: () => [getTasks],
        updateQuery: (updatedTask, oldTasks) =>
          oldTasks &&
          oldTasks.map((task) =>
            task.id === updatedTask.id ? { ...task, ...updatedTask } : task
          ),
      } as OptimisticUpdateDefinition<TaskPayload, Task[]>,
    ],
  })

  if (!task) return <div> Task with id {taskId} does not exist. </div>
  if (isError) return <div> Error occurred! {error.message} </div>

  async function toggleIsDone({ id, isDone }: Task) {
    try {
      updateTaskIsDoneOptimistically({ id, isDone: !isDone })
    } catch (err) {
      console.log(err)
    }
  }

  return (
    <>
      {isFetching ? (
        <div> Fetching task ... </div>
      ) : (
        <>
          <h2>Task</h2>
          <div> id: {task.id} </div>
          <div> description: {task.description} </div>
          <div> is done: {task.isDone ? 'Yes' : 'No'} </div>
          <button onClick={() => toggleIsDone(task)}>
            Mark as {task.isDone ? 'undone' : 'done'}
          </button>
        </>
      )}
      <br />
      <Link to="/">Go to dashboard</Link>
    </>
  )
}

export default Todo
