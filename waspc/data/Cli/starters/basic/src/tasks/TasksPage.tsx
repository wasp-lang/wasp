import { type AuthUser } from "wasp/auth";
import { CreateTaskForm } from "./components/CreateTaskForm";
import { TaskList } from "./components/TaskList";

export const TasksPage = ({ user }: { user: AuthUser }) => {
  return (
    <main className="w-[min(48rem,100%-2rem)] gap-8 self-start">
      <h2 className="font-bold">{`${user.username}'s tasks`}</h2>
      <section className="card p-4 lg:p-6">
        <CreateTaskForm />
      </section>
      <section id="tasks">
        <TaskList />
      </section>
    </main>
  );
};
