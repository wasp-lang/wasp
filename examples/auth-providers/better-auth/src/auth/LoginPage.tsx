import { useState } from "react";
import { authClient } from "./authClient";

/**
 * The only meaningfully different file between the three example apps.
 *
 * Better Auth CAN mint a session server-side, so this page posts credentials and
 * gets a token back. Compare the Clerk example, where that is impossible.
 *
 * The token is then handed to Wasp's own client storage so that every subsequent
 * Wasp API call carries it -- that hand-off is the only Wasp-specific line here.
 */
export function LoginPage() {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [error, setError] = useState<string | null>(null);
  const [isSignup, setIsSignup] = useState(false);

  async function submit(e: React.FormEvent) {
    e.preventDefault();
    setError(null);

    const result = isSignup
      ? await authClient.signUp.email({ email, password, name: email })
      : await authClient.signIn.email({ email, password });

    if (result.error) {
      setError(result.error.message ?? "Something went wrong");
      return;
    }

    const token = result.data?.token;
    if (!token) {
      setError("No session token returned");
      return;
    }

    // Hand Better Auth's token to Wasp's client so `useAuth()` and every
    // operation call pick it up.
    const { setSessionId } = await import("wasp/client/api");
    setSessionId(token);
    window.location.href = "/";
  }

  return (
    <main
      style={{ maxWidth: 380, margin: "3rem auto", fontFamily: "system-ui" }}
    >
      <h1>{isSignup ? "Sign up" : "Log in"}</h1>
      <p style={{ color: "#666" }}>Powered by Better Auth</p>
      <form onSubmit={submit}>
        <input
          type="email"
          value={email}
          onChange={(e) => setEmail(e.target.value)}
          placeholder="email"
        />
        <input
          type="password"
          value={password}
          onChange={(e) => setPassword(e.target.value)}
          placeholder="password"
        />
        <button type="submit">{isSignup ? "Sign up" : "Log in"}</button>
      </form>
      {error ? <p style={{ color: "crimson" }}>{error}</p> : null}
      <button onClick={() => setIsSignup((v) => !v)}>
        {isSignup ? "I already have an account" : "I need an account"}
      </button>
    </main>
  );
}
