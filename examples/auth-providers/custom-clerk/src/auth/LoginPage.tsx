import { useState } from "react";
import { LoginForm, SignupForm } from "wasp/client/auth";

/**
 * Identical to `wasp-auth/`'s login page. This is the file that changes when
 * the app adopts its real auth provider -- everything else stays put.
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
