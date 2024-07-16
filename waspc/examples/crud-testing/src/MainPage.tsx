import { Link, routes } from 'wasp/client/router'
import { logout } from 'wasp/client/auth'
import { getUsername } from 'wasp/auth'

import './Main.css'

import React, { useState } from 'react'
import { tasks as tasksCrud } from 'wasp/client/crud'

const MainPage = () => {
  const { data: tasks, isLoading } = tasksCrud.getAll.useQuery()

  const createTask = tasksCrud.create.useAction()
  const deleteTask = tasksCrud.delete.useAction()
  const updateTask = tasksCrud.update.useAction()

  const [newTaskTitle, setNewTaskTitle] = useState('')
  const [editTaskTitle, setEditTaskTitle] = useState('')
  const [error, setError] = useState('')
  const [isEditing, setIsEditing] = useState<number | null>(null)

  async function handleCreateTask(e: React.FormEvent) {
    setError('')
    e.preventDefault()
    try {
      await createTask({
        title: newTaskTitle,
      })
    } catch (err: unknown) {
      setError(`Error creating task: ${err as any}`)
    }
    setNewTaskTitle('')
  }

  async function handleUpdateTask(e: React.FormEvent) {
    setError('')
    e.preventDefault()
    try {
      await updateTask({ id: isEditing!, title: editTaskTitle })
    } catch (err: unknown) {
      setError('Error updating task.')
    }
    setIsEditing(null)
    setEditTaskTitle('')
  }

  function handleStartEditing(task: { id: number; title: string }) {
    setIsEditing(task.id)
    setEditTaskTitle(task.title)
  }

  async function handleTaskDelete(task: { id: number }) {
    try {
      if (!confirm('Are you sure you want to delete this task?')) {
        return
      }
      await deleteTask({ id: task.id })
    } catch (err: unknown) {
      setError('Error deleting task.')
    }
  }

  return (
    <div className="container">
      <main>
        <h1>Tasks master</h1>
        <div className="error">{error}</div>
        <div className="tasks">
          {isLoading && <div>Loading...</div>}
          {tasks?.map((task) => (
            <div key={task.id} className="task">
              {task.id === isEditing ? (
                <>
                  <form className="new-task-form">
                    <label htmlFor="title">Title</label>
                    <input
                      type="text"
                      required
                      value={editTaskTitle}
                      onChange={(e) => setEditTaskTitle(e.target.value)}
                    />

                    <button type="submit" onClick={handleUpdateTask}>
                      Update task
                    </button>
                  </form>
                </>
              ) : (
                <>
                  <div className="task__title">
                    <Link
                      to="/:id/:something?"
                      params={{ id: task.id, something: 'else' }}
                    >
                      Visit {task.title} at{' '}
                      {routes.DetailRoute.build({
                        params: { id: task.id, something: 'else' },
                      })}{' '}
                      by {getUsername(task.user)}
                    </Link>
                  </div>
                  <button onClick={() => handleTaskDelete(task)}>Delete</button>
                  <a onClick={() => handleStartEditing(task)}>
                    <i>Edit</i>
                  </a>
                </>
              )}
            </div>
          ))}
          {tasks?.length === 0 && <div>No tasks yet.</div>}
        </div>
        <form className="new-task-form">
          <label htmlFor="title">Title</label>
          <input
            type="text"
            required
            value={newTaskTitle}
            onChange={(e) => setNewTaskTitle(e.target.value)}
          />

          <button type="submit" onClick={handleCreateTask}>
            Create task
          </button>
        </form>
        <button onClick={logout}>Logout</button>
      </main>
    </div>
  )
}

export default MainPage
