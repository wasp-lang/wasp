import { useState } from "react";
import { api, setCredential } from "wasp/client/api";

/**
 * Login and signup post to the scheme's own `api()` routes. Login answers
 * with the bearer token the scheme's private issuer minted; adopting it
 * through `setCredential` is what makes every later request carry it, and
 * what tells `logout()` which scheme to sign out of.
 */
export function LoginPage() {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [error, setError] = useState<string | null>(null);
  const [isSignup, setIsSignup] = useState(false);

  async function submit(e: React.FormEvent) {
    e.preventDefault();
    setError(null);

    try {
      if (isSignup) {
        await api.post("/password-auth/signup", {
          json: { email, password },
        });
      }

      const { credential } = await api
        .post("/password-auth/login", { json: { email, password } })
        .json<{ credential: string }>();
      setCredential(credential, "password");
      window.location.href = "/";
    } catch (err) {
      setError(err instanceof Error ? err.message : "Something went wrong");
    }
  }

  return (
    <main
      style={{ maxWidth: 380, margin: "3rem auto", fontFamily: "system-ui" }}
    >
      <h1>{isSignup ? "Sign up" : "Log in"}</h1>
      <p style={{ color: "#666" }}>Hand-rolled password auth</p>
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
