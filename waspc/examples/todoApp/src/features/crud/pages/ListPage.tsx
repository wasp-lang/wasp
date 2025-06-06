import { useState } from "react";

import { getEmail } from "wasp/auth";
import { tasks as tasksCrud } from "wasp/client/crud";
import { Link } from "wasp/client/router";
import { Task } from "wasp/entities";
import { Button } from "../../../components/Button";
import { FeatureContainer } from "../../../components/FeatureContainer";
import { Input } from "../../../components/Input";

type CrudTask = Awaited<ReturnType<typeof tasksCrud.getAll.query>>[number];

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
    <FeatureContainer>
      <div className="space-y-6" data-testid="crud-tasks">
        <div className="flex items-center justify-between">
          <h1 className="text-2xl font-semibold text-gray-900">CRUD Tasks</h1>
        </div>

        {error && (
          <div className="bg-red-50 border border-red-200 text-red-600 px-4 py-3 rounded-lg">
            {error}
          </div>
        )}

        <div className="space-y-4">
          {isLoading && (
            <div className="card text-center py-8 text-gray-500">
              Loading...
            </div>
          )}

          {tasks?.map((task) => (
            <div key={task.id} className="card">
              {task.id === isEditing ? (
                <TaskEdit
                  title={editTaskTitle}
                  onTitleChange={setEditTaskTitle}
                  onSubmit={handleUpdateTask}
                  cancelEdit={() => setIsEditing(null)}
                />
              ) : (
                <TaskView
                  task={task}
                  onEdit={() => handleStartEditing(task)}
                  onDelete={() => handleTaskDelete(task)}
                />
              )}
            </div>
          ))}

          {tasks?.length === 0 && (
            <div
              className="card text-center py-8 text-gray-500"
              data-testid="no-tasks-message"
            >
              No tasks yet. Create your first task below.
            </div>
          )}
        </div>

        <div className="card">
          <h2 className="text-lg font-semibold text-gray-900 mb-4">
            Create New Task
          </h2>
          <form className="space-y-4" onSubmit={handleCreateTask}>
            <Input
              type="text"
              required
              value={newTaskTitle}
              onChange={(e) => setNewTaskTitle(e.target.value)}
              label="Task Description"
              placeholder="Enter task description..."
            />

            <Button type="submit" variant="primary">
              Create task
            </Button>
          </form>
        </div>
      </div>
    </FeatureContainer>
  );
};

function TaskView({
  task,
  onEdit,
  onDelete,
}: {
  task: CrudTask;
  onEdit: () => void;
  onDelete: () => void;
}) {
  return (
    <div className="space-y-3" data-testid="task-view">
      <div>
        <Link
          to="/crud/:id"
          params={{ id: task.id }}
          className="text-lg font-medium text-gray-900 hover:text-primary-600 transition-colors"
          data-testid="text"
        >
          {task.description}
        </Link>
        <p className="text-sm text-gray-500 mt-1" data-testid="created-by">
          Created by {getEmail(task.user) ?? "(no email)"}
        </p>
      </div>

      <div className="flex gap-2 pt-2 border-t border-gray-100">
        <Button onClick={onEdit} variant="secondary">
          Edit
        </Button>
        <Button onClick={onDelete} variant="danger">
          Delete
        </Button>
      </div>
    </div>
  );
}

function TaskEdit({
  cancelEdit,
  title,
  onTitleChange,
  onSubmit,
}: {
  cancelEdit?: () => void;
  title: string;
  onTitleChange: (title: string) => void;
  onSubmit: (e: React.FormEvent) => Promise<void>;
}) {
  return (
    <form
      className="space-y-4"
      onSubmit={onSubmit}
      data-testid="edit-task-form"
    >
      <Input
        type="text"
        required
        value={title}
        onChange={(e) => onTitleChange(e.target.value)}
        label="Task Description"
        data-testid="edit-task-input"
      />

      <div className="flex gap-2">
        <Button type="submit" variant="primary">
          Update task
        </Button>
        <Button onClick={cancelEdit} variant="secondary">
          Cancel
        </Button>
      </div>
    </form>
  );
}
