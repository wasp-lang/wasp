import { useState } from "react";
import { config } from "wasp/client";
import { api, setSessionId } from "wasp/client/api";

/**
 * Login posts a `Basic` credential to Wasp's `POST /auth/login` exchange: the
 * provider's `authenticate` verifies it once, Wasp mints its own session, and
 * the provider is off the request path until logout. Signup posts to the
 * provider's own `api()` route.
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

      // The exchange, addressed to this provider by id: a Basic credential
      // in, a Wasp session out. Hand-rolled fetch because the generated
      // `exchangeCredentialForSession` helper always sends a Bearer
      // credential, and this provider authenticates a Basic one.
      const response = await fetch(
        `${config.apiUrl}/auth/login/${encodeURIComponent("password")}`,
        {
          method: "POST",
          headers: { Authorization: `Basic ${btoa(`${email}:${password}`)}` },
        },
      );
      if (!response.ok) {
        setError("Invalid credentials");
        return;
      }
      const { sessionId } = (await response.json()) as { sessionId: string };
      setSessionId(sessionId, "password");
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
