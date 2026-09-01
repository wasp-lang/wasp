import { useEffect, useState } from "react";

import type {
  ClientAdapterFactory,
  WaspClientRuntime,
} from "@wasp.sh/auth-contract/client";

import type { WaspAuthLibOptions } from "./spec.js";

/**
 * The client half of the externalized wasp auth: plain React exports plus a
 * handful of fetch helpers, all built on the two things Wasp hands a client
 * adapter -- `apiUrl` and the provider-bound `setSession` sink. No generated
 * imports anywhere; the package versions independently of any app.
 */

let runtime: WaspClientRuntime | null = null;
let options: WaspAuthLibOptions | null = null;

export const createClientAdapter: ClientAdapterFactory<WaspAuthLibOptions> = (
  waspRuntime,
  adapterOptions,
) => {
  runtime = waspRuntime;
  options = adapterOptions;
  // No Wrapper, no ambient credential: sessions are adopted explicitly via
  // the setSession sink at the moment a login route returns one.
  return {};
};

function getRuntime(): WaspClientRuntime {
  if (runtime === null) {
    throw new Error(
      "@wasp.sh/auth client used before Wasp instantiated its adapter. Is the provider declared in main.wasp.ts?",
    );
  }
  return runtime;
}

async function post(
  path: string,
  body: Record<string, unknown>,
): Promise<{ ok: boolean; status: number; data: Record<string, unknown> }> {
  const response = await fetch(`${getRuntime().apiUrl}/wasp-auth${path}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  const data = (await response.json().catch(() => ({}))) as Record<
    string,
    unknown
  >;
  return { ok: response.ok, status: response.status, data };
}

async function adoptSessionFrom(data: Record<string, unknown>): Promise<void> {
  if (typeof data.sessionId !== "string") {
    throw new Error("Login did not return a session.");
  }
  await getRuntime().setSession(data.sessionId);
}

// PUBLIC API -- one helper per in-tree client action.

export async function signup(username: string, password: string): Promise<void> {
  const { ok, data } = await post("/username/signup", { username, password });
  if (!ok) throw new Error(String(data.message ?? "Signup failed"));
}

export async function login(username: string, password: string): Promise<void> {
  const { ok, data } = await post("/username/login", { username, password });
  if (!ok) throw new Error(String(data.message ?? "Login failed"));
  await adoptSessionFrom(data);
}

export async function emailSignup(email: string, password: string): Promise<void> {
  const { ok, data } = await post("/email/signup", { email, password });
  if (!ok) throw new Error(String(data.message ?? "Signup failed"));
}

export async function emailLogin(email: string, password: string): Promise<void> {
  const { ok, data } = await post("/email/login", { email, password });
  if (!ok) throw new Error(String(data.message ?? "Login failed"));
  await adoptSessionFrom(data);
}

export async function verifyEmail(token: string): Promise<void> {
  const { ok, data } = await post("/email/verify", { token });
  if (!ok) throw new Error(String(data.message ?? "Verification failed"));
}

export async function requestPasswordReset(email: string): Promise<void> {
  const { ok, data } = await post("/email/request-password-reset", { email });
  if (!ok) throw new Error(String(data.message ?? "Request failed"));
}

export async function resetPassword(
  token: string,
  password: string,
): Promise<void> {
  const { ok, data } = await post("/email/reset-password", { token, password });
  if (!ok) throw new Error(String(data.message ?? "Reset failed"));
}

/** Navigate here to start the Google login dance. */
export function googleLoginUrl(): string {
  return `${getRuntime().apiUrl}/wasp-auth/google/login`;
}

export async function exchangeOAuthCode(code: string): Promise<void> {
  const { ok, data } = await post("/exchange-code", { code });
  if (!ok) throw new Error(String(data.message ?? "Code exchange failed"));
  await adoptSessionFrom(data);
}

// PUBLIC API -- minimal pages, the in-tree Auth UI's job in plainest form.

export function AuthForm({ onSuccess }: { onSuccess?: () => void }) {
  const methods = options?.methods ?? {};
  const [mode, setMode] = useState<"login" | "signup">("login");
  const [identifier, setIdentifier] = useState("");
  const [password, setPassword] = useState("");
  const [message, setMessage] = useState<string | null>(null);
  const useEmail = methods.email !== undefined;

  async function submit(e: React.FormEvent) {
    e.preventDefault();
    setMessage(null);
    try {
      if (useEmail) {
        if (mode === "signup") {
          await emailSignup(identifier, password);
          setMessage("Check your email for a verification link.");
          return;
        }
        await emailLogin(identifier, password);
      } else {
        if (mode === "signup") {
          await signup(identifier, password);
        }
        await login(identifier, password);
      }
      onSuccess?.();
    } catch (error) {
      setMessage(error instanceof Error ? error.message : "Something went wrong");
    }
  }

  return (
    <div style={{ maxWidth: 380, margin: "3rem auto", fontFamily: "system-ui" }}>
      <h1>{mode === "signup" ? "Sign up" : "Log in"}</h1>
      <p style={{ color: "#666" }}>Wasp auth, running from a package</p>
      <form onSubmit={submit}>
        <input
          value={identifier}
          onChange={(e) => setIdentifier(e.target.value)}
          placeholder={useEmail ? "email" : "username"}
        />
        <input
          type="password"
          value={password}
          onChange={(e) => setPassword(e.target.value)}
          placeholder="password"
        />
        <button type="submit">{mode === "signup" ? "Sign up" : "Log in"}</button>
      </form>
      {methods.google !== undefined ? (
        <p>
          <a href={googleLoginUrl()}>Continue with Google</a>
        </p>
      ) : null}
      {message ? <p style={{ color: "crimson" }}>{message}</p> : null}
      <button onClick={() => setMode(mode === "login" ? "signup" : "login")}>
        {mode === "login" ? "I need an account" : "I already have an account"}
      </button>
    </div>
  );
}

export function OAuthCallbackPage({ redirectTo = "/" }: { redirectTo?: string }) {
  const [error, setError] = useState<string | null>(null);
  useEffect(() => {
    const code = window.location.hash.slice(1);
    if (!code) {
      setError("Missing login code.");
      return;
    }
    exchangeOAuthCode(code)
      .then(() => window.location.replace(redirectTo))
      .catch((e) => setError(e instanceof Error ? e.message : "Login failed"));
  }, [redirectTo]);
  return <p style={{ fontFamily: "system-ui" }}>{error ?? "Signing you in..."}</p>;
}

export function VerifyEmailPage({ loginPath = "/login" }: { loginPath?: string }) {
  const [message, setMessage] = useState("Verifying...");
  useEffect(() => {
    const token = new URLSearchParams(window.location.search).get("token");
    if (!token) {
      setMessage("Missing verification token.");
      return;
    }
    verifyEmail(token)
      .then(() => setMessage("Email verified. You can log in now."))
      .catch((e) => setMessage(e instanceof Error ? e.message : "Verification failed"));
  }, []);
  return (
    <p style={{ fontFamily: "system-ui" }}>
      {message} <a href={loginPath}>Log in</a>
    </p>
  );
}

export function PasswordResetPage({ loginPath = "/login" }: { loginPath?: string }) {
  const [password, setPassword] = useState("");
  const [message, setMessage] = useState<string | null>(null);

  async function submit(e: React.FormEvent) {
    e.preventDefault();
    const token = new URLSearchParams(window.location.search).get("token");
    if (!token) {
      setMessage("Missing reset token.");
      return;
    }
    try {
      await resetPassword(token, password);
      setMessage("Password changed. You can log in now.");
    } catch (error) {
      setMessage(error instanceof Error ? error.message : "Reset failed");
    }
  }

  return (
    <div style={{ maxWidth: 380, margin: "3rem auto", fontFamily: "system-ui" }}>
      <h1>Reset password</h1>
      <form onSubmit={submit}>
        <input
          type="password"
          value={password}
          onChange={(e) => setPassword(e.target.value)}
          placeholder="new password"
        />
        <button type="submit">Reset</button>
      </form>
      {message ? (
        <p>
          {message} <a href={loginPath}>Log in</a>
        </p>
      ) : null}
    </div>
  );
}
