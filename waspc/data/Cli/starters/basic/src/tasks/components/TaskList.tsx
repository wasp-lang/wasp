import {
  deleteCompletedTasks,
  getTasks,
  useQuery,
} from "wasp/client/operations";
import { Button } from "../../shared/components/Button";
import { TaskListItem } from "./TaskListItem";

export function TaskList() {
  const { data: tasks, isLoading, isSuccess } = useQuery(getTasks);

  if (isLoading) {
    return <p>Loading...</p>;
  }

  if (!isSuccess) {
    return <p className="text-red-500">Error loading tasks.</p>;
  }

  const completedTasks = tasks.filter((task) => task.isDone);

  async function handleDeleteCompletedTasks() {
    try {
      await deleteCompletedTasks();
    } catch (err: unknown) {
      window.alert(`Error while deleting tasks: ${String(err)}`);
    }
  }

  if (tasks.length === 0) {
    return <p className="text-neutral-500">No tasks found.</p>;
  }

  return (
    <div className="flex flex-col gap-6">
      <ul className="flex flex-col gap-2">
        {tasks.map((task) => (
          <TaskListItem task={task} key={task.id} />
        ))}
      </ul>
      <div className="flex h-5 items-center justify-between">
        <div className="text-sm text-neutral-500">
          <span>
            {tasks.length} {tasks.length === 1 ? "task" : "tasks"}
          </span>
          <span className="mx-2">·</span>
          <span>{completedTasks.length} completed</span>
        </div>
        {completedTasks.length > 0 && (
          <Button
            className="flex items-center gap-2"
            size="sm"
            onClick={handleDeleteCompletedTasks}
          >
            Clear completed
            <svg
              xmlns="http://www.w3.org/2000/svg"
              viewBox="0 -960 960 960"
              className="size-5 fill-black"
            >
              <path d="M280-120q-33 0-56.5-23.5T200-200v-520h-40v-80h200v-40h240v40h200v80h-40v520q0 33-23.5 56.5T680-120H280Zm400-600H280v520h400v-520ZM360-280h80v-360h-80v360Zm160 0h80v-360h-80v360ZM280-720v520-520Z" />
            </svg>
          </Button>
        )}
      </div>
    </div>
  );
}
