import { confirmMerge, linkUsername, unlink } from "@wasp.sh/auth/client";
import { useState } from "react";
import { Link } from "react-router";
import type { AuthUser } from "wasp/auth";

/**
 * Account linking: the signed-in user's login methods, with a form to add
 * another and a button to disconnect each. This app enables only username
 * and password, so the second login is a second username; with OAuth methods
 * enabled, `startOAuthLink("google")` connects a provider the same way.
 */
export function ConnectedAccountsPage({ user }: { user: AuthUser }) {
  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const [error, setError] = useState<string | null>(null);
  // Set when the username belongs to another account whose password the
  // user just proved they know: merging needs their explicit yes.
  const [mergeTicket, setMergeTicket] = useState<string | null>(null);

  async function run(action: () => Promise<void>) {
    setError(null);
    try {
      await action();
    } catch (e) {
      setError(e instanceof Error ? e.message : "Something went wrong");
    }
  }

  return (
    <main
      style={{ maxWidth: 480, margin: "3rem auto", fontFamily: "system-ui" }}
    >
      <h1>Connected accounts</h1>
      <p>
        <Link to="/">Back to tasks</Link>
      </p>

      <ul>
        {user.identities.map((identity) => (
          <li key={`${identity.providerName}/${identity.providerUserId}`}>
            <code>{identity.providerName}</code> {identity.providerUserId}{" "}
            <button onClick={() => run(() => unlink(identity))}>
              Disconnect {identity.providerUserId}
            </button>
          </li>
        ))}
      </ul>

      <form
        onSubmit={(e) => {
          e.preventDefault();
          void run(async () => {
            const result = await linkUsername({ username, password });
            if (result.status === "merge-required") {
              setMergeTicket(result.mergeTicket);
              return;
            }
            setUsername("");
            setPassword("");
          });
        }}
      >
        <input
          name="link-username"
          value={username}
          onChange={(e) => setUsername(e.target.value)}
          placeholder="another username"
        />
        <input
          name="link-password"
          type="password"
          value={password}
          onChange={(e) => setPassword(e.target.value)}
          placeholder="its password"
        />
        <button type="submit">Add login</button>
      </form>
      {mergeTicket !== null ? (
        <p>
          <code>{username}</code> is another account of yours. Merging moves its
          tasks and logins here and deletes it.{" "}
          <button
            onClick={() =>
              run(async () => {
                await confirmMerge(mergeTicket);
                setMergeTicket(null);
                setUsername("");
                setPassword("");
              })
            }
          >
            Merge accounts
          </button>
        </p>
      ) : null}
      {error ? <p style={{ color: "crimson" }}>{error}</p> : null}
    </main>
  );
}
