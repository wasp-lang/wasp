import { LoginForm, SignupForm } from "@wasp.sh/auth/client";
import { useState } from "react";

/**
 * This file is the ONLY meaningful difference between the example apps.
 *
 * Wasp's own auth can mint a session server-side, so its package renders the
 * forms and posts credentials to its own endpoints. Compare
 * `clerk/src/auth/LoginPage.tsx`, where none of this is possible.
 */
export function LoginPage() {
  const [isSignup, setIsSignup] = useState(false);

  return (
    <main
      style={{ maxWidth: 380, margin: "3rem auto", fontFamily: "system-ui" }}
    >
      <h1>{isSignup ? "Sign up" : "Log in"}</h1>
      {isSignup ? <SignupForm /> : <LoginForm />}
      <button onClick={() => setIsSignup((v) => !v)}>
        {isSignup ? "I already have an account" : "I need an account"}
      </button>
    </main>
  );
}
