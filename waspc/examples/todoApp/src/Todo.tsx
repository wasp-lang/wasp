import { Link } from 'wasp/client/router'
import { type Task } from 'wasp/entities'

import {
  useAction,
  type OptimisticUpdateDefinition,
  createTask,
  updateTaskIsDone,
  deleteCompletedTasks,
  toggleAllTasks,
  useQuery,
  getTasks,
  getTask,
  getDate,
  getAnything,
} from 'wasp/client/operations'

import React, { useState, FormEventHandler, ChangeEventHandler } from 'react'

type NonEmptyArray<T> = [T, ...T[]]

export function areThereAnyTasks(
  tasks: Task[] | undefined
): tasks is NonEmptyArray<Task> {
  return !!(tasks && tasks.length > 0)
}

const Todo = () => {
  // logAll()
  const { data: tasks, isError, error: tasksError } = useQuery(getTasks)

  const TasksError = () => {
    return (
      <div>{'Error during fetching tasks: ' + (tasksError?.message || '')}</div>
    )
  }

  return (
    <div className="flex justify-center">
      <div className="w-3/6 shadow-md rounded p-6">
        <h1 className="mb-2">Todos</h1>

        <div className="flex justify-start gap-2">
          <ToggleAllTasksButton disabled={!areThereAnyTasks(tasks)} />
          <NewTaskForm />
        </div>

        {isError && <TasksError />}

        {areThereAnyTasks(tasks) && (
          <>
            <Tasks tasks={tasks} />

            <Footer tasks={tasks} />
          </>
        )}
      </div>
    </div>
  )
}

const Footer = ({ tasks }: { tasks: NonEmptyArray<Task> }) => {
  const numCompletedTasks = tasks.filter((t) => t.isDone).length
  const numUncompletedTasks = tasks.filter((t) => !t.isDone).length

  const handleDeleteCompletedTasks = async () => {
    try {
      await deleteCompletedTasks()
    } catch (err) {
      console.log(err)
    }
  }

  return (
    <div className="flex justify-between">
      <div>{numUncompletedTasks} items left</div>

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

const Tasks = ({ tasks }: { tasks: NonEmptyArray<Task> }) => {
  return (
    <div>
      <table className="border-separate border-spacing-2">
        <tbody>
          {tasks.map((task, idx) => (
            <TaskView task={task} key={idx} />
          ))}
        </tbody>
      </table>
    </div>
  )
}

type UpdateTaskIsDonePayload = Pick<Task, 'id' | 'isDone'>

const TaskView = ({ task }: { task: Task }) => {
  const updateTaskIsDoneOptimistically = useAction(updateTaskIsDone, {
    optimisticUpdates: [
      {
        getQuerySpecifier: () => [getTasks],
        updateQuery: (updatedTask, oldTasks) => {
          if (oldTasks === undefined) {
            // cache is empty
            return [{ ...task, ...updatedTask }]
          } else {
            return oldTasks.map((task) =>
              task.id === updatedTask.id ? { ...task, ...updatedTask } : task
            )
          }
        },
      } as OptimisticUpdateDefinition<UpdateTaskIsDonePayload, Task[]>,
      {
        getQuerySpecifier: () => [getTask, { id: task.id }],
        // This query's cache should should never be emtpy
        updateQuery: ({ isDone }, oldTask) => {
          if (oldTask === undefined) {
            // Cache is empty (e.g., the user has not yet opened the task's
            // dedicated page.
            // Passes the type checker because of the assertion Returning
            // undefined should be properly supported and not a hack, see
            // https://github.com/wasp-lang/wasp/issues/2017
            return undefined
          } else {
            const result = { ...oldTask!, isDone }
            return result
          }
        },
      } as OptimisticUpdateDefinition<UpdateTaskIsDonePayload, Task>,
    ],
  })
  const handleTaskIsDoneChange: ChangeEventHandler<HTMLInputElement> = async (
    event
  ) => {
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
          type="checkbox"
          id={String(task.id)}
          checked={task.isDone}
          onChange={handleTaskIsDoneChange}
          color="default"
        />
      </td>
      <td>
        <Link to="/task/:id" params={{ id: task.id }}>
          {task.description}
        </Link>
      </td>
    </tr>
  )
}

const NewTaskForm = () => {
  const defaultDescription = ''
  const [description, setDescription] = useState(defaultDescription)
  const createTaskFn = useAction(createTask, {
    optimisticUpdates: [
      {
        getQuerySpecifier: () => [getTasks],
        updateQuery: (newTask, oldTasks) => {
          if (oldTasks === undefined) {
            // cache is empty
            return [newTask as Task]
          } else {
            return [...oldTasks, newTask as Task]
          }
        },
      } as OptimisticUpdateDefinition<
        Pick<Task, 'isDone' | 'description'>,
        Task[]
      >,
    ],
  })

  const createNewTask = async (description: Task['description']) => {
    const task = { isDone: false, description }
    await createTaskFn(task)
  }

  const handleNewTaskSubmit: FormEventHandler<HTMLFormElement> = async (
    event
  ) => {
    event.preventDefault()
    try {
      setDescription(defaultDescription)
      await createNewTask(description)
    } catch (err) {
      console.log(err)
    }
  }

  return (
    <form onSubmit={handleNewTaskSubmit} className="content-start flex gap-2">
      <input
        type="text"
        placeholder="Enter task"
        value={description}
        onChange={(e) => setDescription(e.target.value)}
      />
      <button className="btn btn-primary">Create new task</button>
    </form>
  )
}

const ToggleAllTasksButton = ({ disabled }: { disabled: boolean }) => {
  const handleToggleAllTasks = async () => {
    try {
      await toggleAllTasks()
    } catch (err) {
      console.log(err)
    }
  }

  return (
    <button
      className="btn btn-primary"
      disabled={disabled}
      onClick={handleToggleAllTasks}
    >
      ✓
    </button>
  )
}

// Use this function to test calling actions directly
async function logAll() {
  const tasks = await getTasks()
  console.info('Got tasks:', tasks)

  const someId = tasks.map((task) => task.id).find((id) => id)
  if (!someId) {
    console.info('No tasks found')
  } else {
    const task = await getTask({ id: someId })
    console.info(`Got task with id ${someId}`, task)
  }

  const date = await getDate()
  console.info('Got date:', date)

  const anything = await getAnything()
  console.info('Got anything:', anything)
}

export default Todo
