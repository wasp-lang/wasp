import { Link } from "wasp/client/router";
import { type Task } from "wasp/entities";

import { TaskVisibility } from "@prisma/client";
import {
  getTask,
  getTasks,
  updateTaskIsDone,
  useAction,
  useQuery,
  type OptimisticUpdateDefinition,
} from "wasp/client/operations";

import { useParams } from "react-router-dom";
import { cn } from "../../../cn";
import { Button } from "../../../components/Button";
import { FeatureContainer } from "../../../components/FeatureContainer";

type TaskPayload = Pick<Task, "id" | "isDone">;

const VISIBILITY_EXPLANATION = {
  [TaskVisibility.PRIVATE]: "only you",
  [TaskVisibility.LINK_ONLY]: "people with a link",
  [TaskVisibility.PUBLIC]: "everyone",
} as const satisfies Record<TaskVisibility, string>;

export const TaskDetailPage = () => {
  const { id } = useParams();
  const taskId = parseInt(id!);

  const {
    data: task,
    isLoading,
    error,
    isError,
  } = useQuery(getTask, { id: taskId });

  const updateTaskIsDoneOptimistically = useAction(updateTaskIsDone, {
    optimisticUpdates: [
      {
        getQuerySpecifier: () => [getTask, { id: taskId }],
        // Since we're on a page that uses this Query, its query's cache should
        // should never be empty.
        updateQuery: ({ isDone }, oldTask) => ({ ...oldTask!, isDone }),
      } as OptimisticUpdateDefinition<TaskPayload, Task>,
      {
        getQuerySpecifier: () => [getTasks],
        updateQuery: (updatedTask, oldTasks) =>
          oldTasks &&
          oldTasks.map((task) =>
            task.id === updatedTask.id ? { ...task, ...updatedTask } : task,
          ),
      } as OptimisticUpdateDefinition<TaskPayload, Task[]>,
    ],
  });

  if (!task) return <div> Task with id {taskId} does not exist. </div>;
  if (isError) return <div> Error occurred! {error.message} </div>;

  async function toggleIsDone({ id, isDone }: Task) {
    try {
      updateTaskIsDoneOptimistically({ id, isDone: !isDone });
    } catch (err) {
      console.log(err);
    }
  }

  return (
    <FeatureContainer>
      <div className="space-y-4">
        <div className="card" data-testid="task-detail">
          {isLoading ? (
            <div className="text-center py-8 text-gray-500">
              Fetching task...
            </div>
          ) : (
            <div className="space-y-4">
              <div className="flex items-center justify-between">
                <h1 className="text-2xl font-semibold text-gray-900">
                  Task Details
                </h1>
                <span className="text-sm text-gray-500">ID: {task.id}</span>
              </div>

              <div className="space-y-3">
                <div>
                  <label className="text-sm font-medium text-gray-600">
                    Description
                  </label>
                  <p className="text-gray-900 mt-1" data-testid="text">
                    {task.description}
                  </p>
                </div>

                <div>
                  <label className="text-sm font-medium text-gray-600">
                    Visibility
                  </label>
                  <p className="text-gray-900 mt-1" data-testid="visibility">
                    Visible to {VISIBILITY_EXPLANATION[task.visibility]}
                  </p>
                </div>

                <div>
                  <label className="text-sm font-medium text-gray-600">
                    Status
                  </label>
                  <div className="mt-1" data-testid="status">
                    <span
                      className={cn(
                        "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
                        task.isDone
                          ? "bg-green-100 text-green-800 border border-green-200"
                          : "bg-gray-100 text-gray-800 border border-gray-200",
                      )}
                    >
                      {task.isDone ? "Completed" : "Pending"}
                    </span>
                  </div>
                </div>
              </div>

              <div className="pt-4 border-t border-gray-200">
                <Button onClick={() => toggleIsDone(task)} variant="primary">
                  Mark as {task.isDone ? "pending" : "completed"}
                </Button>
              </div>
            </div>
          )}
        </div>

        <div>
          <Link to="/tasks" className="link">
            Go back to tasks
          </Link>
        </div>
      </div>
    </FeatureContainer>
  );
};
