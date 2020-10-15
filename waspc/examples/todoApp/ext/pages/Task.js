import React from 'react'
import { Link } from 'react-router-dom'

import { useQuery } from '@wasp/queries'
import getTask from '@wasp/queries/getTask.js'

const Todo = (props) => {
  const taskId = parseInt(props.match.params.id)
  const { data: task, isFetching, error } =
    useQuery(getTask, { id: taskId })

  if (!task) return <div>Task with id {taskId} does not exist.</div>
  if (error) return <div>Error occurred! {error}</div>

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
        </>
      )}
      <br />
      <Link to='/'>Go to dashboard</Link>
    </>
  )
}

export default Todo
