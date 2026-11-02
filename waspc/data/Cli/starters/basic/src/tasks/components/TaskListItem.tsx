import { ChangeEvent } from "react";
import { twJoin } from "tailwind-merge";
import { updateTaskStatus } from "wasp/client/operations";
import { TagLabel } from "../../tags/components/TagLabel";
import { TaskWithTags } from "../queries";

interface TaskListItemProps {
  task: TaskWithTags;
}

export function TaskListItem({ task }: TaskListItemProps) {
  async function setTaskDone(
    event: ChangeEvent<HTMLInputElement>,
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
      <label className="flex w-full cursor-pointer flex-wrap items-center justify-between gap-4 px-4 py-3">
        <div className="flex min-w-0 items-center gap-4">
          <input
            type="checkbox"
            className="size-5 shrink-0 accent-yellow"
            checked={task.isDone}
            onChange={setTaskDone}
          />
          <div className="flex min-w-0 flex-col wrap-break-word">
            <p className={twJoin(task.isDone && "text-gray line-through")}>
              {task.description}
            </p>
            <span className="text-xs text-gray">
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
