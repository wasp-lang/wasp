import type { ChangeEvent, SubmitEvent } from "react";
import type { AuthUser } from "wasp/auth";
import { logout } from "wasp/client/auth";
import {
  createTask,
  getTasks,
  updateTask,
  useQuery,
} from "wasp/client/operations";
import type { Task } from "wasp/entities";
import Logo from "./assets/wasp-logo-rounded.svg";
import "./Main.css";

export const MainPage = ({ user }: { user: AuthUser }) => {
  const { data: tasks, isLoading, error } = useQuery(getTasks);

  return (
    <>
      <header>
        <h1>
          <img src={Logo} alt="Wasp logo" />
          Todo App
        </h1>
        <button onClick={logout}>Logout</button>
      </header>
      <main>
        <h2>Tasks</h2>
        <NewTaskForm />
        {tasks && <TasksList tasks={tasks} />}

        {isLoading && "Loading..."}
        {error && "Error: " + error}
      </main>
    </>
  );
};

const TaskView = ({ task }: { task: Task }) => {
  const handleIsDoneChange = async (event: ChangeEvent<HTMLInputElement>) => {
    try {
      await updateTask({
        id: task.id,
        isDone: event.target.checked,
      });
    } catch (error: any) {
      window.alert("Error while updating task: " + error.message);
    }
  };

  return (
    <li>
      <label>
        <input
          type="checkbox"
          checked={task.isDone}
          onChange={handleIsDoneChange}
        />
        {task.description}
      </label>
    </li>
  );
};

const TasksList = ({ tasks }: { tasks: Task[] }) => {
  if (!tasks?.length) return <div>No tasks</div>;

  return (
    <ul className="tasks">
      {tasks.map((task) => (
        <TaskView task={task} key={task.id} />
      ))}
    </ul>
  );
};

const NewTaskForm = () => {
  const handleSubmit = async (event: SubmitEvent<HTMLFormElement>) => {
    event.preventDefault();
    try {
      const target = event.target as HTMLFormElement;
      const description = target.description.value;
      target.reset();
      await createTask({ description });
    } catch (err: any) {
      window.alert("Error: " + err.message);
    }
  };

  return (
    <form className="new-task-form" onSubmit={handleSubmit}>
      <input
        name="description"
        type="text"
        placeholder="What needs to be done?"
        defaultValue=""
      />
      <input type="submit" value="Create task" />
    </form>
  );
};
