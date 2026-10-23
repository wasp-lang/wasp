import { type AuthUser } from "wasp/auth";
import { CreateTaskForm } from "./components/CreateTaskForm";
import { TaskList } from "./components/TaskList";

export const TasksPage = ({ user }: { user: AuthUser }) => {
  return (
    <div className="mx-auto my-12 flex w-[min(48rem,100%-2rem)] flex-col gap-8">
      <h2 className="text-2xl font-bold uppercase">{`${user.username}'s tasks`}</h2>
      <section className="card p-4 lg:p-6">
        <CreateTaskForm />
      </section>
      <section id="tasks">
        <TaskList />
      </section>
    </div>
  );
};
