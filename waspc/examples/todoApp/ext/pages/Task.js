import React from 'react'
import { Link } from "react-router-dom"

import { useQuery } from '@wasp/queries'
import getTask from '@wasp/queries/getTask.js'

const Todo = (props) => {

  const { data: task, isFetching, isError }
    = useQuery(getTask, { id: parseInt(props.match.params.id) })

  if (!task) {
    return (
      <div>Task with id {props.match.params.id} does not exist.</div>
    )
  }

  if (isError) {
    return (
      <div>Error occurred!</div>
    )
  }

  return (
    <>
      { isFetching ? (
        <div>
          Fetching task ...
        </div>
      ) : (
        <>
          <h2>Task</h2>
          <div>
            id: {task.id}
          </div>
          <div>
            description: {task.description}
          </div>
          <div>
            is done: {task.isDone ? 'Yes' : 'No'}
          </div>
        </>
      )}
      <br/>
    <Link to="/">Go to dashboard</Link>
    </>
  )
}

export default Todo

