import { FormEvent, ChangeEvent } from 'react'
import getTasks from '@wasp/queries/getTasks'
import createTask from '@wasp/actions/createTask'
import updateTask from '@wasp/actions/updateTask'
import { useQuery } from '@wasp/queries'
import { Task } from '@wasp/entities'
import logout from '@wasp/auth/logout'

const MainPage = () => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

  return (
    <div>
      <NewTaskForm />

      {tasks && <TasksList tasks={tasks} />}

      {isLoading && 'Loading...'}
      {error && 'Error: ' + error}
      <button onClick={logout}>Logout</button>
    </div>
  )
}

const Task = ({ task }: { task: Task }) => {
  const handleIsDoneChange = async (event: ChangeEvent<HTMLInputElement>) => {
    try {
      await updateTask({
        id: task.id,
        isDone: event.target.checked,
      })
    } catch (error: any) {
      window.alert('Error while updating task: ' + error.message)
    }
  }

  return (
    <div>
      <input
        type="checkbox"
        id={String(task.id)}
        checked={task.isDone}
        onChange={handleIsDoneChange}
      />
      {task.description}
    </div>
  )
}

const TasksList = ({ tasks }: { tasks: Task[] }) => {
  if (!tasks?.length) return <div>No tasks</div>

  return (
    <div>
      {tasks.map((task, idx) => (
        <Task task={task} key={idx} />
      ))}
    </div>
  )
}

const NewTaskForm = () => {
  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault()
    try {
      const target = event.target as HTMLFormElement
      const description = target.description.value
      target.reset()
      await createTask({ description })
    } catch (err: any) {
      window.alert('Error: ' + err.message)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      <input name="description" type="text" defaultValue="" />
      <input type="submit" value="Create task" />
    </form>
  )
}

export default MainPage
