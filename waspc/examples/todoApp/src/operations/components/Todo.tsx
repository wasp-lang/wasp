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
import { Button } from "../../components/Button";
import { Input } from "../../components/Input";
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
    <>
      <div className="shadow-md rounded p-6 bg-white">
        <h1 className="mb-2 text-2xl font-medium">Todos</h1>

        <div className="flex justify-start gap-2">
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

      <div className="text-sm text-gray-600 mt-4">
        {getTotalTaskCountMessage(numTasks)}
      </div>
    </>
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
    <div className="flex justify-between">
      <div>{numUncompletedTasks} items left</div>

      <div>
        <Button
          onClick={handleDeleteCompletedTasks}
          variant="danger"
          className={numCompletedTasks > 0 ? "" : "hidden"}
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
      <table className="border-separate border-spacing-2">
        <tbody>
          {tasks.map((task, idx) => (
            <TaskView task={task} key={idx} />
          ))}
        </tbody>
      </table>
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
    <tr>
      <td>
        <input
          type="checkbox"
          id={String(task.id)}
          checked={task.isDone}
          onChange={handleTaskIsDoneChange}
          color="default"
          className="appearance-none h-4 w-4 border border-gray-300 rounded-sm bg-white checked:bg-blue-600 checked:border-blue-600 focus:outline-none transition duration-200 mt-1 align-top bg-no-repeat bg-center bg-contain float-left mr-2 cursor-pointer"
        />
      </td>
      <td>
        <Link to="/task/:id" params={{ id: task.id }}>
          {task.description} {email && `by ${email}`}
        </Link>
      </td>
    </tr>
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
    <form onSubmit={handleNewTaskSubmit} className="content-start flex gap-2">
      <Input
        type="text"
        placeholder="Enter task"
        value={description}
        onChange={(e) => setDescription(e.target.value)}
      />
      <Button type="submit">Create new task</Button>
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
