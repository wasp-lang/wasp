import './Main.css'
import React, { useEffect, FormEventHandler, FormEvent } from 'react'
import {
  createTask,
  // customEmailSending,
  deleteTasks,
  getTasks,
  useQuery,
} from 'wasp/client/operations'
import waspLogo from './waspLogo.png'
import type { Task } from 'wasp/entities'
import { AuthUser, getFirstProviderUserId } from 'wasp/auth'
import { Link } from 'react-router-dom'
import { Tasks } from 'wasp/client/crud'
// import login from 'wasp/auth/login'
// import signup from 'wasp/auth/signup'
import { Todo } from './Todo'
import { logout, useAuth } from 'wasp/client/auth'

export const MainPage = ({ user }: { user: AuthUser }) => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)
  const { data: userAgain } = useAuth()

  const { data: allTasks } = Tasks.getAll.useQuery()

  if (isLoading) return 'Loading...'
  if (error) return 'Error: ' + error

  // console.log(login)
  // console.log(signup)

  const completed = tasks?.filter((task) => task.isDone).map((task) => task.id)

  return (
    <main>
      <img src={waspLogo} alt="wasp logo" />
      {/* <button onClick={() => customEmailSending(undefined)}>
        customEmailSending
      </button> */}
      <Link to="/chat">Wonna chat?</Link>
      {user && (
        <h1>
          {user.getFirstProviderUserId()}
          {`'s tasks :)`}
        </h1>
      )}
      <NewTaskForm />
      {tasks && <TasksList tasks={tasks} />}
      <h2>Everything</h2>
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
