import { twJoin } from "tailwind-merge";
import { updateTaskStatus } from "wasp/client/operations";
import { TagLabel } from "../../tags/components/TagLabel";
import { TaskWithTags } from "../queries";

interface TaskListItemProps {
  task: TaskWithTags;
}

export function TaskListItem({ task }: TaskListItemProps) {
  async function setTaskDone(
    event: React.ChangeEvent<HTMLInputElement>,
  ): Promise<void> {
    try {
      await updateTaskStatus({
        id: task.id,
        isDone: event.currentTarget.checked,
      });
    } catch (err: unknown) {
      window.alert(`Error while updating task: ${String(err)}`);
    }
  }

  return (
    <li>
      <label
        className={twJoin(
          "card flex w-full flex-wrap items-center justify-between gap-4 rounded-lg px-6 py-3",
          task.isDone ? "bg-primary-50" : "bg-neutral-50",
        )}
      >
        <div className="flex min-w-0 items-center gap-4">
          <input
            type="checkbox"
            className="h-5 w-5 shrink-0 accent-primary-500"
            checked={task.isDone}
            onChange={setTaskDone}
          />
          <div className="flex min-w-0 flex-col break-words">
            <p>{task.description}</p>
            <span className="text-xs text-neutral-500">
              {task.createdAt.toLocaleDateString()}
            </span>
          </div>
        </div>
        <ul className="flex flex-wrap gap-x-2">
          {task.tags.map((tag) => (
            <li key={tag.id}>
              <TagLabel tag={tag} isActive={true} size="tiny" />
            </li>
          ))}
        </ul>
      </label>
    </li>
  );
}
