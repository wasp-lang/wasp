import { Task } from "wasp/entities";

import { TaskVisibility } from "@prisma/client";
import { cn } from "../cn";

export function TaskDetailView({
  task,
}: {
  task: Pick<Task, "description" | "visibility" | "isDone">;
}) {
  const VISIBILITY_EXPLANATION: Record<TaskVisibility, string> = {
    [TaskVisibility.PRIVATE]: "only you",
    [TaskVisibility.LINK_ONLY]: "people with a link",
    [TaskVisibility.PUBLIC]: "everyone",
  };

  return (
    <div className="space-y-3" data-testid="task-detail-view">
      <TaskDetailItem label="Description">
        <p className="text-gray-900" data-testid="description">
          {task.description}
        </p>
      </TaskDetailItem>

      <TaskDetailItem label="Visibility">
        <p className="text-gray-900" data-testid="visibility">
          Visible to {VISIBILITY_EXPLANATION[task.visibility]}
        </p>
      </TaskDetailItem>

      <TaskDetailItem label="Status">
        <div data-testid="status">
          <span
            className={cn(
              "inline-flex items-center rounded-full px-2.5 py-0.5 text-xs font-medium",
              task.isDone
                ? "border border-green-200 bg-green-100 text-green-800"
                : "border border-gray-200 bg-gray-100 text-gray-800",
            )}
          >
            {task.isDone ? "Completed" : "Pending"}
          </span>
        </div>
      </TaskDetailItem>
    </div>
  );
}

function TaskDetailItem({
  label,
  children,
}: React.PropsWithChildren<{ label: string }>) {
  return (
    <div className="space-y-1">
      <label className="text-sm font-medium text-gray-600">{label}</label>
      {children}
    </div>
  );
}
