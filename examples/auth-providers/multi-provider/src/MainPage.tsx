import { useState } from "react";
import { Link } from "react-router";
import { logout, useAuth } from "wasp/client/auth";
import { createTask, getMyTasks, useQuery } from "wasp/client/operations";

/**
 * The uniform surface, now with two providers behind it. `useAuth()` and
 * `logout()` work the same however the session was minted;
 * `user.sessionProviderId` says which provider vouched for this login.
 */
export function MainPage() {
  const { data: user } = useAuth();
  const { data: tasks, isLoading, refetch } = useQuery(getMyTasks);
  const [description, setDescription] = useState("");

  return (
    <main
      style={{ maxWidth: 480, margin: "3rem auto", fontFamily: "system-ui" }}
    >
      <header style={{ display: "flex", justifyContent: "space-between" }}>
        <h1>Tasks</h1>
        <button onClick={logout}>Log out</button>
      </header>

      {/* `user.id` is this app's own User.id, never the provider's id. */}
      <p>
        Signed in as <code>{user?.id}</code> via{" "}
        <code>{user?.sessionProviderId}</code>
      </p>
      <p>
        <Link to="/admin">Admin report</Link> (wasp-authenticated sessions only)
      </p>

      <form
        onSubmit={async (e) => {
          e.preventDefault();
          if (!description.trim()) return;
          await createTask({ description });
          setDescription("");
          void refetch();
        }}
      >
        <input
          value={description}
          onChange={(e) => setDescription(e.target.value)}
          placeholder="What needs doing?"
        />
        <button type="submit">Add</button>
      </form>

      {isLoading ? <p>Loading…</p> : null}
      <ul>{tasks?.map((task) => <li key={task.id}>{task.description}</li>)}</ul>
    </main>
  );
}
