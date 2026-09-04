import { SignIn } from "@wasp.sh/auth-clerk/client";
import { useState } from "react";
import { LoginForm, SignupForm } from "@wasp.sh/auth/client";

/**
 * One login page, two independent front doors, composed by the app.
 *
 * Wasp ships no meta-login-chrome across providers: the app decides how the
 * audiences are presented. Wasp's own forms post credentials to Wasp's own
 * endpoints; Clerk's component talks to Clerk's Frontend API, and completing
 * it fires a credential event the generated client exchanges for a Wasp
 * session (when none exists yet).
 */
export function LoginPage() {
  const [isSignup, setIsSignup] = useState(false);

  return (
    <main
      style={{
        display: "flex",
        gap: "3rem",
        justifyContent: "center",
        marginTop: "3rem",
        fontFamily: "system-ui",
      }}
    >
      <section style={{ maxWidth: 380 }}>
        <h2>Team accounts</h2>
        {isSignup ? <SignupForm /> : <LoginForm />}
        <button onClick={() => setIsSignup((v) => !v)}>
          {isSignup ? "I already have an account" : "I need an account"}
        </button>
      </section>
      <section>
        <h2>Customer accounts</h2>
        <SignIn />
      </section>
    </main>
  );
}
