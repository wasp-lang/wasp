import React from 'react'
import { Link } from 'react-router-dom'

import { useQuery } from '@wasp/queries'
import { useAction } from '@wasp/actions'
import updateTaskIsDone from '@wasp/actions/updateTaskIsDone'
import getTask from '@wasp/queries/getTask.js'
import getTasks from '@wasp/queries/getTasks.js'

const Todo = (props) => {
  const taskId = parseInt(props.match.params.id)
  const { data: task, isFetching, error } = useQuery(getTask, { id: taskId })

  const updateTaskIsDone = useAction(updateTaskIsDone, {
    optimisticUpdates: [
      {
        getQuery: () => [getTask, { id: taskId }],
        updateQuery: ({ isDone }, oldTask) => ({ ...oldTask, isDone }),
      },
      {
        getQuery: getTasks,
        updateQuery: (updatedTask, oldTasks) =>
          oldTasks.map(task => 
            task.id === updatedTask.id ? { ...task, updatedTask } : task
          ),
      }
    ]
  })

  if (!task) return <div>Task with id {taskId} does not exist.</div>
  if (error) return <div>Error occurred! {error}</div>

  async function toggleIsDone() {
    try {
      updateTaskIsDoneAction.mutateAsync({ id: task.id, isDone: !task.isDone })
    } catch (err) {
      window.alert("Error: " + err.message)
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
          <button onClick={toggleIsDone}>Toggle</button>
        </>
      )}
      <br />
      <Link to='/'>Go to dashboard</Link>
    </>
  )
}

export default Todo
