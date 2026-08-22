import { useState } from "react";
import { LoginForm, SignupForm } from "wasp/client/auth";

/**
 * This file is the ONLY meaningful difference between the three example apps.
 *
 * Wasp's own auth can mint a session server-side, so Wasp renders the forms and
 * posts credentials to its own endpoints. Compare `clerk/src/auth/LoginPage.tsx`,
 * where none of this is possible.
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
