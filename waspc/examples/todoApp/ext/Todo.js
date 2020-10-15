import React, { useState } from 'react'
import { Link } from 'react-router-dom'

import { useQuery } from '@wasp/queries'
import getTasks from '@wasp/queries/getTasks.js'
import createTask from '@wasp/actions/createTask.js'
import updateTaskIsDone from '@wasp/actions/updateTaskIsDone.js'
import deleteCompletedTasks from '@wasp/actions/deleteCompletedTasks.js'
import toggleAllTasks from '@wasp/actions/toggleAllTasks.js'

const Todo = (props) => {
  const { data: tasks, isError, error: tasksError } = useQuery(getTasks)

  const isThereAnyTask = () => tasks?.length > 0

  const TasksError = (props) => {
    return 'Error during fetching tasks: ' + (tasksError?.message || '')
  }

  return (
    <div className='todos'>
      <div className='todos__container'>
        <h1> Todos </h1>

        <div className='todos__toggleAndInput'>
          <ToggleAllTasksButton disabled={!isThereAnyTask()} />
          <NewTaskForm />
        </div>

        {isError && <TasksError />}

        {isThereAnyTask() && (
          <>
            <Tasks tasks={tasks} />

            <Footer tasks={tasks} />
          </>
        )}
      </div>
    </div>
  )
}

const Footer = (props) => {
  const numCompletedTasks = props.tasks.filter(t => t.isDone).length
  const numUncompletedTasks = props.tasks.filter(t => !t.isDone).length

  const handleDeleteCompletedTasks = async () => {
    try {
      await deleteCompletedTasks()
    } catch (err) {
      console.log(err)
      window.alert('Error:' + err.message)
    }
  }

  return (
    <div className='todos__footer'>
      <div className='todos__footer__itemsLeft'>
        {numUncompletedTasks} items left
      </div>

      <div className='todos__footer__clearCompleted'>
        <button
          className={numCompletedTasks > 0 ? '' : 'hidden'}
          onClick={handleDeleteCompletedTasks}
        >
          Delete completed
        </button>
      </div>
    </div>
  )
}

const Tasks = (props) => {
  return (
    <div>
      {props.tasks.map((task, idx) => <Task task={task} key={idx} />)}
    </div>
  )
}

const Task = (props) => {
  const handleTaskIsDoneChange = async (event) => {
    const taskId = parseInt(event.target.id)
    const newIsDoneVal = event.target.checked

    try {
      await updateTaskIsDone({ taskId, newIsDoneVal })
    } catch (err) {
      console.log(err)
      window.alert('Error:' + err.message)
    }
  }

  return (
    <div>
      <input
        type='checkbox'
        id={props.task.id}
        checked={props.task.isDone}
        onChange={handleTaskIsDoneChange}
      />
      <Link to={`/task/${props.task.id}`}> {props.task.description} </Link>
    </div>
  )
}

const NewTaskForm = (props) => {
  const defaultDescription = ''
  const [description, setDescription] = useState(defaultDescription)

  const createNewTask = async (description) => {
    const task = { isDone: false, description }
    await createTask(task)
  }

  const handleNewTaskSubmit = async (event) => {
    event.preventDefault()
    try {
      await createNewTask(description)
      setDescription(defaultDescription)
    } catch (err) {
      console.log(err)
      window.alert('Error:' + err.message)
    }
  }

  return (
    <form onSubmit={handleNewTaskSubmit}>
      <input
        type='text'
        value={description}
        onChange={e => setDescription(e.target.value)}
      />
      <input type='submit' value='Create new task' />
    </form>
  )
}

const ToggleAllTasksButton = (props) => {
  const handleToggleAllTasks = async () => {
    try {
      await toggleAllTasks()
    } catch (err) {
      console.log(err)
      window.alert('Error:' + err.message)
    }
  }

  return (
    <button
      disabled={props.disabled}
      onClick={handleToggleAllTasks}
    >
      ✓
    </button>
  )
}

export default Todo
