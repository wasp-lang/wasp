import { getNumTasks, useQuery } from "wasp/client/operations";
import { Link } from "wasp/client/router";

import {
  createTask,
  deleteCompletedTasks,
  getTasks,
  toggleAllTasks,
  updateTaskIsDone,
  useAction,
  type OptimisticUpdateDefinition,
} from "wasp/client/operations";

import { ChangeEventHandler, FormEventHandler, useState } from "react";
import { getEmail } from "wasp/auth";
import { cn } from "../../../cn";
import { Button } from "../../../components/Button";
import { Input } from "../../../components/Input";
import { getTotalTaskCountMessage } from "../helpers";

type NonEmptyArray<T> = [T, ...T[]];

type TaskWithUser = Awaited<ReturnType<typeof getTasks>>[number];

export function areThereAnyTasks(
  tasks: TaskWithUser[] | undefined,
): tasks is NonEmptyArray<TaskWithUser> {
  return !!(tasks && tasks.length > 0);
}

const Todo = () => {
  const { data: tasks, isError, error: tasksError } = useQuery(getTasks);
  const { data: numTasks } = useQuery(getNumTasks);

  const TasksError = () => {
    return (
      <div>{"Error during fetching tasks: " + (tasksError?.message || "")}</div>
    );
  };

  return (
    <div className="space-y-4">
      <h2 className="feature-title">Todos</h2>
      <div className="card">
        <div className="flex items-center gap-2">
          <ToggleAllTasksButton disabled={!areThereAnyTasks(tasks)} />
          <NewTaskForm />
        </div>

        {isError && <TasksError />}

        {areThereAnyTasks(tasks) && (
          <>
            <Tasks tasks={tasks} />

            <Footer tasks={tasks} />
          </>
        )}
      </div>

      <div className="mt-4 text-sm text-gray-600">
        {getTotalTaskCountMessage(numTasks)}
      </div>
    </div>
  );
};

const Footer = ({ tasks }: { tasks: NonEmptyArray<TaskWithUser> }) => {
  const numCompletedTasks = tasks.filter((t) => t.isDone).length;
  const numUncompletedTasks = tasks.filter((t) => !t.isDone).length;

  const handleDeleteCompletedTasks = async () => {
    try {
      await deleteCompletedTasks();
    } catch (err) {
      console.log(err);
    }
  };

  return (
    <div className="flex items-center justify-between border-t pt-4">
      <div className="py-3 text-sm">{numUncompletedTasks} item(s) left</div>

      <div>
        <Button
          onClick={handleDeleteCompletedTasks}
          variant="secondary"
          className={cn(numCompletedTasks === 0 && "hidden")}
        >
          Delete completed
        </Button>
      </div>
    </div>
  );
};

const Tasks = ({ tasks }: { tasks: NonEmptyArray<TaskWithUser> }) => {
  return (
    <div>
      {tasks.map((task, idx) => (
        <TaskView task={task} key={idx} />
      ))}
    </div>
  );
};

type UpdateTaskIsDonePayload = Pick<TaskWithUser, "id" | "isDone">;

const TaskView = ({ task }: { task: TaskWithUser }) => {
  const updateTaskIsDoneOptimistically = useAction(updateTaskIsDone, {
    optimisticUpdates: [
      {
        getQuerySpecifier: () => [getTasks],
        updateQuery: (updatedTask, oldTasks) => {
          if (oldTasks === undefined) {
            // cache is empty
            return [{ ...task, ...updatedTask }];
          } else {
            return oldTasks.map((task) =>
              task.id === updatedTask.id ? { ...task, ...updatedTask } : task,
            );
          }
        },
      } as OptimisticUpdateDefinition<UpdateTaskIsDonePayload, TaskWithUser[]>,
    ],
  });
  const handleTaskIsDoneChange: ChangeEventHandler<HTMLInputElement> = async (
    event,
  ) => {
    const id = parseInt(event.target.id);
    const isDone = event.target.checked;

    try {
      await updateTaskIsDoneOptimistically({ id, isDone });
    } catch (err) {
      console.log(err);
    }
  };

  const email = getEmail(task.user);

  return (
    <div
      className="flex items-center gap-4 border-b py-4 last:border-b-0"
      data-testid="task-view"
    >
      <div>
        <input
          type="checkbox"
          id={String(task.id)}
          checked={task.isDone}
          onChange={handleTaskIsDoneChange}
          className="text-primary-600 focus:ring-primary-500 h-4 w-4 cursor-pointer rounded border-gray-300 disabled:cursor-not-allowed disabled:border-gray-200 disabled:bg-gray-50 disabled:opacity-50"
        />
      </div>
      <div>
        <Link
          to="/tasks/:id"
          params={{ id: task.id }}
          className="link"
          data-testid="text"
        >
          {task.description}
        </Link>
        {email && (
          <p className="mt-1 text-sm text-gray-500" data-testid="created-by">
            Created by {email}
          </p>
        )}
      </div>
    </div>
  );
};

const NewTaskForm = () => {
  const defaultDescription = "";
  const [description, setDescription] = useState(defaultDescription);
  const createTaskFn = useAction(createTask, {
    optimisticUpdates: [
      {
        getQuerySpecifier: () => [getTasks],
        updateQuery: (newTask, oldTasks) => {
          const newTaskWithUser = {
            ...newTask,
            user: {},
          } as TaskWithUser;

          if (oldTasks === undefined) {
            // cache is empty
            return [newTaskWithUser];
          } else {
            return [...oldTasks, newTaskWithUser];
          }
        },
      } as OptimisticUpdateDefinition<
        Pick<TaskWithUser, "isDone" | "description">,
        TaskWithUser[]
      >,
    ],
  });

  const createNewTask = async (description: TaskWithUser["description"]) => {
    const task = { isDone: false, description };
    await createTaskFn(task);
  };

  const handleNewTaskSubmit: FormEventHandler<HTMLFormElement> = async (
    event,
  ) => {
    event.preventDefault();
    try {
      setDescription(defaultDescription);
      await createNewTask(description);
    } catch (err) {
      console.log(err);
    }
  };

  return (
    <form
      onSubmit={handleNewTaskSubmit}
      className="grid grid-cols-[1fr_auto] gap-2"
    >
      <Input
        type="text"
        placeholder="Enter task"
        value={description}
        onChange={(e) => setDescription(e.target.value)}
      />
      <Button type="submit">Create task</Button>
    </form>
  );
};

const ToggleAllTasksButton = ({ disabled }: { disabled: boolean }) => {
  const handleToggleAllTasks = async () => {
    try {
      await toggleAllTasks();
    } catch (err) {
      console.log(err);
    }
  };

  return (
    <Button disabled={disabled} onClick={handleToggleAllTasks}>
      ✓
    </Button>
  );
};

export default Todo;
