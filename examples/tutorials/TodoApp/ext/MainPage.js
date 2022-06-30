import React from 'react'

import logout from '@wasp/auth/logout.js'
import { useQuery } from '@wasp/queries'
import getTasks from '@wasp/queries/getTasks'
import createTask from '@wasp/actions/createTask'
import updateTask from '@wasp/actions/updateTask'
import Clocks from './Clocks'
import WaspSourceHeader from './WaspSourceHeader'

const MainPage = () => {
  const { data: tasks, isFetching, error } = useQuery(getTasks)

  return (
    <div>
      <br/>
      <NewTaskForm />

      {tasks && <TasksList tasks={tasks} />}

      <div> <Clocks /> </div>

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

const NewTaskForm = () => {
  const handleSubmit = async (event) => {
    event.preventDefault()
    try {
      const description = event.target.description.value
      event.target.reset()
      await createTask({ description })
    } catch (err) {
      window.alert('Error: ' + err.message)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      <input
        name='description'
        type='text'
        defaultValue=''
      />
      <input type='submit' value='Create task' />
    </form>
  )
}

export default MainPage
