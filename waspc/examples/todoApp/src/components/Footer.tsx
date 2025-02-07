import { NonEmptyArray } from "@util"
import { deleteCompletedTasks } from "wasp/client/operations"
import { Task } from "wasp/entities"

export const Footer = ({ tasks }: { tasks: NonEmptyArray<Task> }) => {
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
