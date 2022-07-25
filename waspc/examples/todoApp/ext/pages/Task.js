import React from 'react'
import { Link, useParams } from 'react-router-dom'

import { useQuery } from '@wasp/queries'
import updateTaskIsDone from '@wasp/actions/updateTaskIsDone'
import getTask from '@wasp/queries/getTask.js'

const Todo = (props) => {
  const params = useParams()
  const taskId = parseInt(params.id)
  const { data: task, isFetching, error } = useQuery(getTask, { id: taskId })

  if (!task) return <div>Task with id {taskId} does not exist.</div>
  if (error) return <div>Error occurred! {error}</div>

  async function toggleIsDone() {
    try {
      updateTaskIsDone({ id: task.id, isDone: !task.isDone })
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
          <button onClick={toggleIsDone}>Mark as {task.isDone ? 'undone' : 'done'}</button>
        </>
      )}
      <br />
      <Link to='/'>Go to dashboard</Link>
    </>
  )
}

export default Todo
