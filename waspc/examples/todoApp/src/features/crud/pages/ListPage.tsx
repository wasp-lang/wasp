import { useState } from "react";

import { getEmail } from "wasp/auth";
import { tasks as tasksCrud } from "wasp/client/crud";
import { Link } from "wasp/client/router";
import { Button } from "../../../components/Button";
import { FeatureContainer } from "../../../components/FeatureContainer";
import { Input } from "../../../components/Input";

type CrudTask = Awaited<ReturnType<typeof tasksCrud.getAll.query>>[number];

export const ListPage = () => {
  const { data: tasks, isLoading } = tasksCrud.getAll.useQuery();

  const createTask = tasksCrud.create.useAction();
  const deleteTask = tasksCrud.delete.useAction();
  const updateTask = tasksCrud.update.useAction();

  const [error, setError] = useState("");
  const [isEditing, setIsEditing] = useState<number | null>(null);

  async function handleCreateTask(data: { description: string }) {
    setError("");
    try {
      await createTask(data);
    } catch (err: unknown) {
      setError("Error creating task.");
    }
  }

  async function handleUpdateTask(data: { description: string }) {
    setError("");
    try {
      await updateTask({
        id: isEditing as number,
        description: data.description,
      });
      setIsEditing(null);
    } catch (err: unknown) {
      setError("Error updating task.");
    }
  }

  async function handleTaskDelete(task: { id: number }) {
    setError("");
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
                  initialTaskDescription={task.description}
                  onUpdate={handleUpdateTask}
                  cancelEdit={() => setIsEditing(null)}
                />
              ) : (
                <TaskView
                  task={task}
                  onEdit={() => setIsEditing(task.id)}
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
          <TaskCreateForm onCreate={handleCreateTask} />
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
  initialTaskDescription,
  cancelEdit,
  onUpdate,
}: {
  initialTaskDescription: string;
  cancelEdit?: () => void;
  onUpdate: ({ description }: { description: string }) => Promise<void>;
}) {
  const [editTaskDescription, setEditTaskDescription] = useState(
    initialTaskDescription,
  );

  function onSubmit(e: React.FormEvent) {
    e.preventDefault();
    onUpdate({ description: editTaskDescription });
  }

  return (
    <form
      className="space-y-4"
      onSubmit={onSubmit}
      data-testid="edit-task-form"
    >
      <Input
        type="text"
        required
        value={editTaskDescription}
        onChange={(e) => setEditTaskDescription(e.target.value)}
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

function TaskCreateForm({
  onCreate,
}: {
  onCreate: ({ description }: { description: string }) => Promise<void>;
}) {
  const [newTaskDescription, setNewTaskDescription] = useState("");

  function onSubmit(e: React.FormEvent) {
    e.preventDefault();
    onCreate({ description: newTaskDescription });
    setNewTaskDescription("");
  }

  return (
    <form className="space-y-4" onSubmit={onSubmit}>
      <Input
        type="text"
        required
        value={newTaskDescription}
        onChange={(e) => setNewTaskDescription(e.target.value)}
        label="Task Description"
        placeholder="Enter task description..."
      />

      <Button type="submit" variant="primary">
        Create task
      </Button>
    </form>
  );
}
