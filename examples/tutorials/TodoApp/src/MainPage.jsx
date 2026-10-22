import { logout } from "wasp/client/auth";
import {
  createTask,
  getTasks,
  updateTask,
  useQuery,
} from "wasp/client/operations";
import Logo from "./assets/wasp-logo-rounded.svg";
import "./Main.css";

export const MainPage = ({ user }) => {
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

const TaskView = ({ task }) => {
  const handleIsDoneChange = async (event) => {
    try {
      await updateTask({
        id: task.id,
        isDone: event.target.checked,
      });
    } catch (error) {
      window.alert("Error while updating task: " + error.message);
    }
  };

  return (
    <li>
      <input
        type="checkbox"
        id={String(task.id)}
        checked={task.isDone}
        onChange={handleIsDoneChange}
      />
      <label htmlFor={String(task.id)}>{task.description}</label>
    </li>
  );
};

const TasksList = ({ tasks }) => {
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
  const handleSubmit = async (event) => {
    event.preventDefault();
    try {
      const target = event.target;
      const description = target.description.value;
      target.reset();
      await createTask({ description });
    } catch (err) {
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
