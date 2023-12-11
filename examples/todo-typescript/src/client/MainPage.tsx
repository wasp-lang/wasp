import React, { FormEventHandler, FormEvent } from "react";
import waspLogo from "./waspLogo.png";

import "./Main.css";
// Wasp imports 🐝 = }
import logout from "@wasp/auth/logout";
import { useQuery } from "@wasp/queries"; // Wasp uses a thin wrapper around react-query
import getTasks from "@wasp/queries/getTasks";
import createTask from "@wasp/actions/createTask";
import updateTask from "@wasp/actions/updateTask";
import deleteTasks from "@wasp/actions/deleteTasks";
import type { Task, User } from "@wasp/entities";

export const MainPage = ({ user }: { user: User }) => {
  const { data: tasks, isLoading, error } = useQuery(getTasks);

  if (isLoading) return "Loading...";
  if (error) return "Error: " + error;

  const completed = tasks?.filter((task) => task.isDone).map((task) => task.id);

  return (
    <main>
      <img src={waspLogo} alt="wasp logo" />
      {user && (
        <h1>
          {user.username}
          {`'s tasks :)`}
        </h1>
      )}
      <NewTaskForm />
      {tasks && <TasksList tasks={tasks} />}
      <div className="buttons">
        <button
          className="logout"
          onClick={() => void deleteTasks(completed ?? [])}
        >
          Delete completed
        </button>
        <button className="logout" onClick={logout}>
          Logout
        </button>
      </div>
    </main>
  );
};

function Todo({ id, isDone, description }: Task) {
  const handleIsDoneChange: FormEventHandler<HTMLInputElement> = async (
    event
  ) => {
    try {
      await updateTask({
        id,
        isDone: event.currentTarget.checked,
      });
    } catch (err: any) {
      window.alert("Error while updating task " + err?.message);
    }
  };

  return (
    <li>
      <span className="todo-item">
        <input
          type="checkbox"
          id={id.toString()}
          checked={isDone}
          onChange={handleIsDoneChange}
        />
        <span>{description}</span>
        <button onClick={() => void deleteTasks([id])}>Delete</button>
      </span>
    </li>
  );
}

function TasksList({ tasks }: { tasks: Task[] }) {
  if (tasks.length === 0) return <p>No tasks yet.</p>;
  return (
    <ol className="tasklist">
      {tasks.map((task, idx) => (
        <Todo {...task} key={idx} />
      ))}
    </ol>
  );
}

function NewTaskForm() {
  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();

    try {
      const description = event.currentTarget.description.value;
      console.log(description);
      event.currentTarget.reset();
      await createTask({ description });
    } catch (err: any) {
      window.alert("Error: " + err?.message);
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <input name="description" type="text" defaultValue="" />
      <input type="submit" value="Create task" />
    </form>
  );
}
