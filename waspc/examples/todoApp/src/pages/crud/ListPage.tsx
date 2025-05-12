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
      <main className="space-y-4">
        <h1>Tasks (CRUD feature)</h1>
        {error && <div className="p-4 text-red-500">{error}</div>}
        <div>
          {isLoading && <div>Loading...</div>}
          {tasks?.map((task) => (
            <div key={task.id} className="task">
              {task.id === isEditing ? (
                <>
                  <form className="space-y-4 border p-4">
                    <div className="space-y-2">
                      <label htmlFor="title" className="block">
                        Title
                      </label>
                      <input
                        type="text"
                        required
                        value={editTaskTitle}
                        onChange={(e) => setEditTaskTitle(e.target.value)}
                        data-testid="edit-task-input"
                      />
                    </div>

                    <div className="space-x-2">
                      <button
                        type="submit"
                        onClick={handleUpdateTask}
                        className="btn btn-primary"
                      >
                        Update task
                      </button>
                      <button
                        onClick={() => setIsEditing(null)}
                        className="btn"
                      >
                        Cancel
                      </button>
                    </div>
                  </form>
                </>
              ) : (
                <div className="p-4 border">
                  <div className="mb-2">
                    <Link to="/crud/:id" params={{ id: task.id }}>
                      {task.description} by {getEmail(task.user)}
                    </Link>
                  </div>
                  <div className="flex gap-2">
                    <button
                      onClick={() => handleTaskDelete(task)}
                      className="btn btn-red"
                    >
                      Delete
                    </button>
                    <button
                      onClick={() => handleStartEditing(task)}
                      className="btn"
                    >
                      Edit
                    </button>
                  </div>
                </div>
              )}
            </div>
          ))}
          {tasks?.length === 0 && <div>No tasks yet.</div>}
        </div>
        <form className="space-y-4 border p-4">
          <div className="space-y-2">
            <label htmlFor="title" className="block">
              Title
            </label>
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
