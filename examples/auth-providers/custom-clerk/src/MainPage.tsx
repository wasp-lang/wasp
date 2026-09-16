import { useClerk } from "@clerk/clerk-react";
import { useState } from "react";
import { logout, useAuth } from "wasp/client/auth";
import { createTask, getMyTasks, useQuery } from "wasp/client/operations";

/**
 * Identical in all three example apps.
 *
 * `useAuth()` and `logout()` are uniform across providers: reading who the user is,
 * and dropping the credential, work the same everywhere. Only *establishing* a
 * session differs, and that lives in each app's login page.
 */
export function MainPage() {
  const clerk = useClerk();
  const { data: user } = useAuth();
  const { data: tasks, isLoading, refetch } = useQuery(getMyTasks);
  const [description, setDescription] = useState("");

  return (
    <main
      style={{ maxWidth: 480, margin: "3rem auto", fontFamily: "system-ui" }}
    >
      <header style={{ display: "flex", justifyContent: "space-between" }}>
        <h1>Tasks</h1>
        {/* Server-side sign-out through Wasp, then Clerk's own client
            state -- what the packaged adapter's `onLogout` does for you. */}
        <button
          onClick={async () => {
            await logout();
            await clerk.signOut();
          }}
        >
          Log out
        </button>
      </header>

      {/* `user.id` is this app's own User.id, never the provider's id. */}
      <p>
        Signed in as <code>{user?.id}</code>
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
