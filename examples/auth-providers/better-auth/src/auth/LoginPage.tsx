import { useState } from "react";
import { authClient } from "./authClient";

/**
 * The only meaningfully different file between the three example apps.
 *
 * Better Auth CAN mint a session server-side, so this page posts credentials and
 * gets a token back. Compare the Clerk example, where that is impossible.
 *
 * Better Auth's token IS the credential: the adapter's client stores it with
 * Wasp, every subsequent request carries it, and the Better Auth handler
 * verifies it. Nothing here is Wasp-specific beyond the redirect.
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

    if (!result.data?.token) {
      setError("No session token returned");
      return;
    }

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
