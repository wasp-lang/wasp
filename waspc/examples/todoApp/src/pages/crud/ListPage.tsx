import { useState } from 'react'

import { tasks as tasksCrud } from 'wasp/client/crud'
import { Link } from 'wasp/client/router'
import { getEmail } from 'wasp/auth'
import { Task } from 'wasp/entities'

export const ListPage = () => {
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
        description: newTaskTitle,
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
      await updateTask({
        id: isEditing as number,
        description: editTaskTitle,
      })
    } catch (err: unknown) {
      setError('Error updating task.')
    }
    setIsEditing(null)
    setEditTaskTitle('')
  }

  function handleStartEditing(task: Pick<Task, 'id' | 'description'>) {
    setIsEditing(task.id)
    setEditTaskTitle(task.description)
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
        <h1>Tasks (CRUD feature)</h1>
        <div className="error">{error}</div>
        <div className="tasks">
          {isLoading && <div>Loading...</div>}
          {tasks?.map((task) => (
            <div key={task.id} className="task">
              {task.id === isEditing ? (
                <>
                  <form className="edit-task-form">
                    <label htmlFor="title">Title</label>
                    <div>
                      <input
                        type="text"
                        required
                        value={editTaskTitle}
                        onChange={(e) => setEditTaskTitle(e.target.value)}
                        data-testid="edit-task-input"
                      />
                    </div>

                    <button
                      type="submit"
                      onClick={handleUpdateTask}
                      className="btn"
                    >
                      Update task
                    </button>
                  </form>
                </>
              ) : (
                <>
                  <div className="task__title">
                    <Link to="/crud/:id" params={{ id: task.id }}>
                      {task.description} by {getEmail(task.user)}
                    </Link>
                  </div>
                  <button
                    onClick={() => handleTaskDelete(task)}
                    className="btn btn-red"
                  >
                    Delete
                  </button>
                  <a onClick={() => handleStartEditing(task)} className="btn">
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
          <div>
            <input
              type="text"
              required
              value={newTaskTitle}
              onChange={(e) => setNewTaskTitle(e.target.value)}
            />
          </div>

          <button
            type="submit"
            onClick={handleCreateTask}
            className="btn btn-primary"
          >
            Create task
          </button>
        </form>
      </main>
    </div>
  )
}
