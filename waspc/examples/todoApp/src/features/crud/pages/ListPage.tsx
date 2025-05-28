import { useState } from "react";

import { getEmail } from "wasp/auth";
import { tasks as tasksCrud } from "wasp/client/crud";
import { Link } from "wasp/client/router";
import { Task } from "wasp/entities";
import { Button } from "../../../components/Button";
import { Input } from "../../../components/Input";
import { SimplePageContainer } from "../../../components/SimplePageContainer";

export const ListPage = () => {
  const { data: tasks, isLoading } = tasksCrud.getAll.useQuery();

  const createTask = tasksCrud.create.useAction();
  const deleteTask = tasksCrud.delete.useAction();
  const updateTask = tasksCrud.update.useAction();

  const [newTaskTitle, setNewTaskTitle] = useState("");
  const [editTaskTitle, setEditTaskTitle] = useState("");
  const [error, setError] = useState("");
  const [isEditing, setIsEditing] = useState<number | null>(null);

  async function handleCreateTask(e: React.FormEvent) {
    setError("");
    e.preventDefault();
    try {
      await createTask({
        description: newTaskTitle,
      });
    } catch (err: unknown) {
      setError(`Error creating task: ${err as any}`);
    }
    setNewTaskTitle("");
  }

  async function handleUpdateTask(e: React.FormEvent) {
    setError("");
    e.preventDefault();
    try {
      await updateTask({
        id: isEditing as number,
        description: editTaskTitle,
      });
    } catch (err: unknown) {
      setError("Error updating task.");
    }
    setIsEditing(null);
    setEditTaskTitle("");
  }

  function handleStartEditing(task: Pick<Task, "id" | "description">) {
    setIsEditing(task.id);
    setEditTaskTitle(task.description);
  }

  async function handleTaskDelete(task: { id: number }) {
    try {
      if (!confirm("Are you sure you want to delete this task?")) {
        return;
      }
      await deleteTask({ id: task.id });
    } catch (err: unknown) {
      setError("Error deleting task.");
    }
  }

  return (
    <SimplePageContainer>
      <main className="space-y-4">
        <h1 className="text-2xl font-medium">Crud Tasks List</h1>
        {error && <div className="p-4 text-red-500">{error}</div>}
        <div className="tasks space-y-2">
          {isLoading && <div className="card">Loading...</div>}
          {tasks?.map((task) => (
            <div key={task.id} className="card">
              {task.id === isEditing ? (
                <>
                  <form className="space-y-4">
                    <div className="space-y-2">
                      <Input
                        type="text"
                        required
                        value={editTaskTitle}
                        onChange={(e) => setEditTaskTitle(e.target.value)}
                        label="Title"
                        data-testid="edit-task-input"
                      />
                    </div>

                    <div className="space-x-2">
                      <Button type="submit" onClick={handleUpdateTask}>
                        Update task
                      </Button>
                      <Button
                        onClick={() => setIsEditing(null)}
                        variant="secondary"
                      >
                        Cancel
                      </Button>
                    </div>
                  </form>
                </>
              ) : (
                <div>
                  <div className="mb-2">
                    <Link to="/crud/:id" params={{ id: task.id }}>
                      {task.description} by{" "}
                      {getEmail(task.user) ?? "(no email)"}
                    </Link>
                  </div>
                  <div className="flex gap-2">
                    <Button
                      onClick={() => handleTaskDelete(task)}
                      variant="danger"
                    >
                      Delete
                    </Button>
                    <Button
                      onClick={() => handleStartEditing(task)}
                      variant="secondary"
                    >
                      Edit
                    </Button>
                  </div>
                </div>
              )}
            </div>
          ))}
          {tasks?.length === 0 && <div className="card">No tasks yet.</div>}
        </div>
        <form className="space-y-4 card">
          <div className="space-y-2">
            <Input
              type="text"
              required
              value={newTaskTitle}
              onChange={(e) => setNewTaskTitle(e.target.value)}
              label="Title"
            />
          </div>

          <Button type="submit" onClick={handleCreateTask}>
            Create task
          </Button>
        </form>
      </main>
    </SimplePageContainer>
  );
};
