import React, { useState } from 'react'
import { Link } from 'react-router-dom'

import { useQuery } from '@wasp/queries'
import { useAction } from '@wasp/actions'
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
    <div className='flex justify-center'>
      <div className='w-3/6 shadow-md rounded p-6'>
        <h1>Todos</h1>

        <div className='flex justify-start'>
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
    }
  }

  return (
    <div className='flex justify-between'>
      <div>
        {numUncompletedTasks} items left
      </div>

      <div>
        <button
          className={'btn btn-red ' + (numCompletedTasks > 0 ? '' : 'hidden')}
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
        <table className='border-separate border-spacing-2'>
          <tbody>
            {props.tasks.map((task, idx) => <Task task={task} key={idx} />)}
          </tbody>
        </table>
    </div>
  )
}

const Task = (props) => {
  const updateTaskIsDoneOptimistically = useAction(updateTaskIsDone, {
    optimisticUpdates: [{
      getQuerySpecifier: () => [getTasks],
      updateQuery: (updatedTask, oldTasks) => {
        if (oldTasks === undefined) {
          // cache is empty
          return [updatedTask];
        } else {
          return oldTasks.map(task => task.id === updatedTask.id ? { ...task, ...updatedTask } : task)
        }
      }
    }]
  });
  const handleTaskIsDoneChange = async (event) => {
    const id = parseInt(event.target.id)
    const isDone = event.target.checked

    try {
      await updateTaskIsDoneOptimistically({ id, isDone })
    } catch (err) {
      console.log(err)
    }
  }

  return (
    <tr>
      <td>
        <input
          type='checkbox'
          id={String(props.task.id)}
          checked={props.task.isDone}
          onChange={handleTaskIsDoneChange}
          color='default'
        />
      </td>
      <td>
        <Link to={`/task/${props.task.id}`}> {props.task.description} </Link>
      </td>
    </tr>
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
    }
  }

  return (
    <form onSubmit={handleNewTaskSubmit} className='content-start'>
      <input
        type='text'
        placeholder='Enter task'
        value={description}
        onChange={e => setDescription(e.target.value)}
      />
      <button className='btn btn-blue'>
        Create new task
      </button>
    </form>
  )
}

const ToggleAllTasksButton = (props) => {
  const handleToggleAllTasks = async () => {
    try {
      await toggleAllTasks()
    } catch (err) {
      console.log(err)
    }
  }

  return (
    <button
      className='btn btn-blue'
      disabled={props.disabled}
      onClick={handleToggleAllTasks}
    >
      ✓
    </button>
  )
}

export default Todo
