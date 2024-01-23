import './Main.css'
import React, { useEffect, FormEventHandler, FormEvent } from 'react'
import logout from 'wasp/auth/logout'
import { useQuery, useAction } from 'wasp/rpc' // Wasp uses a thin wrapper around react-query
import { getTasks } from 'wasp/rpc/queries'
import { createTask, updateTask, deleteTasks } from 'wasp/rpc/actions'
import waspLogo from './waspLogo.png'
import type { Task } from 'wasp/entities'
import type { User } from 'wasp/auth/types'
import { getFirstProviderUserId } from 'wasp/auth/user'
import { Tasks } from 'wasp/crud/Tasks'

export const MainPage = ({ user }: { user: User }) => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

  const { data: allTasks } = Tasks.getAll.useQuery()

  if (isLoading) return 'Loading...'
  if (error) return 'Error: ' + error

  const completed = tasks?.filter((task) => task.isDone).map((task) => task.id)

  return (
    <main>
      <img src={waspLogo} alt="wasp logo" />
      {user && (
        <h1>
          {getFirstProviderUserId(user)}
          {`'s tasks :)`}
        </h1>
      )}
      <NewTaskForm />
      {tasks && <TasksList tasks={tasks} />}
      <h2>All</h2>
      {allTasks && <TasksList tasks={allTasks} />}
      <div className="buttons">
        <button
          className="logout"
          onClick={() => void deleteTasks(completed ?? [])}
        >
          Delete completed
        </button>
        <button className="logout" onClick={logout}>
          Logout
        </button>
      </div>
    </main>
  )
}

function Todo({ id, isDone, description }: Task) {
  const handleIsDoneChange: FormEventHandler<HTMLInputElement> = async (
    event
  ) => {
    try {
      await updateTask({
        id,
        isDone: event.currentTarget.checked,
      })
    } catch (err: any) {
      window.alert('Error while updating task ' + err?.message)
    }
  }

  return (
    <li>
      <span className="todo-item">
        <input
          type="checkbox"
          id={id.toString()}
          checked={isDone}
          onChange={handleIsDoneChange}
        />
        <span>{description}</span>
        <button onClick={() => void deleteTasks([id])}>Delete</button>
      </span>
    </li>
  )
}

function TasksList({ tasks }: { tasks: Task[] }) {
  if (tasks.length === 0) return <p>No tasks yet.</p>
  return (
    <ol className="tasklist">
      {tasks.map((task, idx) => (
        <Todo {...task} key={idx} />
      ))}
    </ol>
  )
}

function NewTaskForm() {
  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault()

    try {
      const description = event.currentTarget.description.value
      console.log(description)
      event.currentTarget.reset()
      await createTask({ description })
    } catch (err: any) {
      window.alert('Error: ' + err?.message)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      <input name="description" type="text" defaultValue="" />
      <input type="submit" value="Create task" />
    </form>
  )
}
