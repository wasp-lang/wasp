import React from 'react'
import { Link } from 'react-router-dom'

import { useQuery } from '@wasp/queries'
import { useAction } from '@wasp/actions'
import updateTaskIsDone from '@wasp/actions/updateTaskIsDone'
import getTask from '@wasp/queries/getTask.js'
import getTasks from '@wasp/queries/getTasks.js'
import { Task } from '@wasp/entities'

const Todo = (props: any) => {
  const taskId = parseInt(props.match.params.id)
  const { data: task, isFetching, error } = useQuery<unknown, Task, Task | Task[]>(getTask, { id: taskId })

  const updateTaskIsDoneOptimistically = useAction(updateTaskIsDone, {
    optimisticUpdates: [
      {
        getQuerySpecifier: () => [getTask, { id: taskId }],
        // This query's cache should should never be emtpy
        updateQuery: ({ isDone }: Pick<Task, "id" | "isDone">, oldTask: Task) => ({ ...oldTask, isDone }),
      },
      {
        getQuerySpecifier: () => [getTasks],
        updateQuery: (updatedTask: Task, oldTasks: Task[]) => {
          if (oldTasks === undefined) {
            // cache is empty
            return [updatedTask]
          } else {
            return oldTasks.map(task => 
              task.id === updatedTask.id ? { ...task, ...updatedTask } : task
            )
          }
        },
      }
    ]
  })

  if (!task) return <div>Task with id {taskId} does not exist.</div>
  if (error) return <div>Error occurred! {error}</div>

  async function toggleIsDone() {
    try {
      updateTaskIsDoneOptimistically({ id: task.id, isDone: !task.isDone })
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
          <button onClick={toggleIsDone}>Mark as {task.isDone ? 'undone' : 'done'}</button>
        </>
      )}
      <br />
      <Link to='/'>Go to dashboard</Link>
    </>
  )
}

export default Todo
