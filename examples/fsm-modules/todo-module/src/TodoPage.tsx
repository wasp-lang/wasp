import React, { useState, useEffect } from "react";
import { Link } from "react-router";
import {
  useQuery,
  getTodos,
  createTodo,
  updateTodo,
  deleteTodo,
} from "wasp/client/operations";
import { todoCrud } from "wasp/client/crud";
import { api } from "wasp/client/api";
import { useAuth } from "wasp/client/auth";
import type { Todo } from "./store.js";

type Stats = {
  total: number;
  done: number;
  pending: number;
};

export default function TodoPage() {
  const { data: user } = useAuth();
  const { data: todos, isLoading, error } = useQuery(getTodos);
  const [newText, setNewText] = useState("");

  // Custom API endpoint
  const [stats, setStats] = useState<Stats | null>(null);
  const [statsError, setStatsError] = useState<string | null>(null);

  const fetchStats = async () => {
    try {
      const res = await api.get("/api/todos/stats");
      setStats(res.data);
      setStatsError(null);
    } catch (err: any) {
      setStatsError(err.message ?? "Failed to fetch stats");
    }
  };

  useEffect(() => {
    fetchStats();
  }, [todos]);

  // CRUD hooks
  const {
    data: crudTodos,
    isLoading: crudLoading,
    error: crudError,
  } = todoCrud.getAll.useQuery();

  const handleCreate = async () => {
    await createTodo({ text: newText || "New todo" });
    setNewText("");
  };

  const handleToggle = async (id: number, isDone: boolean) => {
    await updateTodo({ id, isDone: !isDone });
  };

  const handleDelete = async (id: number) => {
    await deleteTodo({ id });
  };

  return (
    <div style={{ maxWidth: 480, margin: "2rem auto", padding: "0 16px" }}>
      <Link to="/" style={{ color: "#555", textDecoration: "none", fontSize: 14 }}>&larr; Back to home</Link>
      {user && (
        <p style={{ color: "#555" }}>
          Hello, {user.getFirstProviderUserId() ?? "User"}!
        </p>
      )}
      <h1>Todos</h1>
      <div style={{ display: "flex", gap: 8, marginBottom: 16 }}>
        <input
          value={newText}
          onChange={(e) => setNewText(e.target.value)}
          placeholder="What needs to be done?"
          onKeyDown={(e) => e.key === "Enter" && handleCreate()}
          style={{ flex: 1, padding: 8 }}
        />
        <button onClick={handleCreate}>Add</button>
      </div>
      {isLoading && <p>Loading todos...</p>}
      {error && <p style={{ color: "red" }}>Error: {error.message}</p>}
      <ul style={{ listStyle: "none", padding: 0 }}>
        {todos?.map((todo: Todo) => (
          <li
            key={todo.id}
            style={{
              display: "flex",
              alignItems: "center",
              gap: 8,
              padding: "8px 0",
              borderBottom: "1px solid #eee",
            }}
          >
            <input
              type="checkbox"
              checked={todo.isDone}
              onChange={() => handleToggle(todo.id, todo.isDone)}
            />
            <span
              style={{
                flex: 1,
                textDecoration: todo.isDone ? "line-through" : "none",
                color: todo.isDone ? "#999" : "inherit",
              }}
            >
              {todo.text}
            </span>
            <button onClick={() => handleDelete(todo.id)}>✕</button>
          </li>
        ))}
      </ul>
      {todos?.length === 0 && (
        <p style={{ color: "#999", textAlign: "center" }}>No todos yet. Add one above!</p>
      )}

      {/* Section 2: Stats (custom API endpoint) */}
      <h2 style={{ marginTop: 32 }}>Stats (via API)</h2>
      {statsError && <p style={{ color: "red" }}>Error: {statsError}</p>}
      {stats ? (
        <div style={{ display: "flex", gap: 24 }}>
          <div>
            <strong>{stats.total}</strong> total
          </div>
          <div>
            <strong>{stats.done}</strong> done
          </div>
          <div>
            <strong>{stats.pending}</strong> pending
          </div>
        </div>
      ) : (
        !statsError && <p>Loading stats...</p>
      )}

      {/* Section 3: CRUD Cards (todoCrud hooks) */}
      <h2 style={{ marginTop: 32 }}>CRUD Cards</h2>
      {crudLoading && <p>Loading CRUD data...</p>}
      {crudError && <p style={{ color: "red" }}>Error: {crudError.message}</p>}
      <div style={{ display: "flex", flexWrap: "wrap", gap: 12 }}>
        {crudTodos?.map((todo: Todo) => (
          <div
            key={todo.id}
            style={{
              border: "1px solid #ddd",
              borderRadius: 8,
              padding: 12,
              minWidth: 120,
              background: todo.isDone ? "#f0f0f0" : "#fff",
            }}
          >
            <div style={{ fontWeight: "bold" }}>{todo.text}</div>
            <div style={{ color: "#666", fontSize: 12 }}>
              {todo.isDone ? "Done" : "Pending"}
            </div>
          </div>
        ))}
      </div>
      {crudTodos?.length === 0 && (
        <p style={{ color: "#999", textAlign: "center" }}>No CRUD items.</p>
      )}
    </div>
  );
}
