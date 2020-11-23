import React, { useState } from 'react'
import { Link } from 'react-router-dom'
import useAuth from '@wasp/auth/useAuth.js'

import logout from '@wasp/auth/logout.js'
import { useQuery } from '@wasp/queries'
import getTasks from '@wasp/queries/getTasks'
import createTask from '@wasp/actions/createTask'
import updateTask from '@wasp/actions/updateTask'
import Clocks from './Clocks'

const MainPage = () => {
  const { data: tasks, isFetching, error } = useQuery(getTasks)

  const { data: user } = useAuth()
  if (!user) {
    return <span> Please <Link to='/login'>log in</Link>. </span>
  }

  return (
    <div>
      <NewTaskForm />

      {tasks && <TasksList tasks={tasks} />}

      <p> <Clocks /> </p>

      {isFetching && 'Fetching...'}
      {error && 'Error: ' + error}

      <button onClick={logout}> Logout </button>
    </div>
  )
}

const Task = (props) => {
  const handleIsDoneChange = async (event) => {
    try {
      await updateTask({
        taskId: props.task.id,
        data: { isDone: event.target.checked }
      })
    } catch (error) {
      window.alert('Error while updating task: ' + error.message)
    }
  }

  return (
    <div>
      <input
        type='checkbox' id={props.task.id}
        checked={props.task.isDone}
        onChange={handleIsDoneChange}
      />
      {props.task.description}
    </div>
  )
}

const TasksList = (props) => {
  if (!props.tasks?.length) return 'No tasks'
  return props.tasks.map((task, idx) => <Task task={task} key={idx} />)
}

const NewTaskForm = (props) => {
  const defaultDescription = ''
  const [description, setDescription] = useState(defaultDescription)

  const handleSubmit = async (event) => {
    event.preventDefault()
    try {
      await createTask({ description })
      setDescription(defaultDescription)
    } catch (err) {
      window.alert('Error: ' + err.message)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      <input
        type='text'
        value={description}
        onChange={e => setDescription(e.target.value)}
      />
      <input type='submit' value='Create task' />
    </form>
  )
}

export default MainPage
