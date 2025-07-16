import { Link } from "wasp/client/router";
import { type Task } from "wasp/entities";

import {
  getTask,
  getTasks,
  updateTaskIsDone,
  useAction,
  useQuery,
  type OptimisticUpdateDefinition,
} from "wasp/client/operations";

import { useParams } from "react-router-dom";
import { Button } from "../../../components/Button";
import { FeatureContainer } from "../../../components/FeatureContainer";
import { TaskDetailView } from "../../../components/TaskDetailView";

type TaskPayload = Pick<Task, "id" | "isDone">;

export const TaskDetailPage = () => {
  const { id } = useParams();
  const taskId = parseInt(id!);

  const {
    data: task,
    isLoading,
    isSuccess,
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
        <div className="flex items-center justify-between">
          <h2 className="feature-title">Task Details</h2>
          {isSuccess && (
            <span className="text-sm text-gray-500">ID: {task.id}</span>
          )}
        </div>
        <div className="card" data-testid="task-detail">
          {isLoading && <div className="text-gray-500">Fetching task...</div>}
          {isSuccess && (
            <div className="space-y-4">
              <TaskDetailView task={task} />

              <div className="border-t border-gray-200 pt-4">
                <Button onClick={() => toggleIsDone(task)} variant="primary">
                  Mark as {task.isDone ? "pending" : "completed"}
                </Button>
              </div>
            </div>
          )}
          {isError && (
            <div className="text-red-500">
              Error fetching task: {error.message}
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
