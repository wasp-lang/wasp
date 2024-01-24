import './Main.css'
import React, { FormEvent } from 'react'
import logout from 'wasp/auth/logout'
import { useQuery } from 'wasp/rpc' // Wasp uses a thin wrapper around react-query
import { getTasks } from 'wasp/rpc/queries'
import { createTask, deleteTasks } from 'wasp/rpc/actions'
import waspLogo from './waspLogo.png'
import type { Task } from 'wasp/entities'
import type { User } from 'wasp/auth/types'
import { getFirstProviderUserId } from 'wasp/auth/user'
import { Todo } from './Todo'

export const MainPage = ({ user }: { user: User }) => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

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
