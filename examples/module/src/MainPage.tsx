import { useState } from "react";
import { addRandomTodo, getTodoItems, useQuery } from "wasp/client/operations";
import type { TodoItems } from "./types";

export function MainPage() {
  const { data, isLoading, error } = useQuery(getTodoItems);
  const [isAdding, setIsAdding] = useState(false);
  const [addError, setAddError] = useState<string | null>(null);

  const todoItems: TodoItems | undefined = data;

  async function handleAddRandomTodo() {
    setIsAdding(true);
    setAddError(null);

    try {
      await addRandomTodo();
    } catch (error) {
      setAddError(error instanceof Error ? error.message : String(error));
    } finally {
      setIsAdding(false);
    }
  }

  return (
    <main style={{ padding: "3rem", fontFamily: "system-ui, sans-serif" }}>
      <p style={{ margin: 0, color: "#666", textTransform: "uppercase" }}>
        Wasp module
      </p>
      <h1 style={{ margin: "0.5rem 0" }}>TODOs from a Wasp module</h1>
      <p style={{ maxWidth: "36rem", lineHeight: 1.6 }}>
        This reusable module reads and writes the host app's Prisma models.
      </p>
      <section
        style={{
          marginTop: "2rem",
          padding: "1rem",
          border: "1px solid #ddd",
          borderRadius: "0.75rem",
          maxWidth: "36rem",
        }}
      >
        <div
          style={{
            display: "flex",
            justifyContent: "space-between",
            gap: "1rem",
            alignItems: "center",
          }}
        >
          <div>
            <h2 style={{ margin: "0 0 0.25rem" }}>TODO list</h2>
            <p style={{ margin: 0, color: "#666" }}>
              {todoItems ? `${todoItems.totalCount} total TODOs` : "Loading..."}
            </p>
          </div>
          <button
            type="button"
            onClick={handleAddRandomTodo}
            disabled={isAdding}
            style={{
              border: 0,
              borderRadius: "999px",
              padding: "0.7rem 1rem",
              background: "#111",
              color: "#fff",
              cursor: isAdding ? "not-allowed" : "pointer",
            }}
          >
            {isAdding ? "Adding..." : "Add one random TODO"}
          </button>
        </div>

        {isLoading && <p>Loading TODOs from the host app...</p>}
        {error && <p>Failed to load TODOs: {error.message}</p>}
        {addError && <p>Failed to add TODO: {addError}</p>}

        {todoItems && (
          <>
            {todoItems.items.length === 0 ? (
              <p>No TODOs yet. Add one from a random quote.</p>
            ) : (
              <ol style={{ paddingLeft: "1.25rem", lineHeight: 1.6 }}>
                {todoItems.items.map((item) => (
                  <li key={item.id}>
                    <span
                      style={{
                        textDecoration: item.isDone ? "line-through" : "none",
                      }}
                    >
                      {item.description}
                    </span>
                  </li>
                ))}
              </ol>
            )}
            <small>Showing latest {todoItems.items.length} TODOs.</small>
          </>
        )}
      </section>
    </main>
  );
}
