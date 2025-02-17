import { useState } from 'react'
import { AuthUser } from 'wasp/auth'
import { Tasks } from 'wasp/client/crud'

export function CrudTestPage({ user }: { user: AuthUser }) {
  const { data: tasks } = Tasks.getAll.useQuery({ filter: 'special filter ' })
  const createTask = Tasks.create.useAction()

  const [newTaskDescription, setNewTaskDescription] = useState('')

  return (
    <div>
      <h1>Tasks</h1>
      <div>
        <input
          type="text"
          value={newTaskDescription}
          onChange={(e) => setNewTaskDescription(e.target.value)}
        />
        <button
          onClick={() =>
            createTask({
              user: { connect: { id: user.id } },
              description: newTaskDescription,
              isDone: false,
            })
          }
        >
          Create task
        </button>
      </div>
      <ul>
        {tasks?.map((task) => (
          <li key={task.id}>{task.description}</li>
        ))}
      </ul>
    </div>
  )
}
