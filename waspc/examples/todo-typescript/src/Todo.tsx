import { FormEventHandler } from 'react'
import { Task } from 'wasp/entities'
import { updateTask, deleteTasks } from 'wasp/client/operations'

export function Todo({ id, isDone, description }: Task) {
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

export function areThereAnyTasks(tasks: Task[]) {
  return tasks.length > 0
}
