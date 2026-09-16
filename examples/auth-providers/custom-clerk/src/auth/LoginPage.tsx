import { SignIn } from "@clerk/clerk-react";

/**
 * The Clerk login page, and the clearest illustration of where Wasp's uniform
 * line falls.
 *
 * There is no `<LoginForm />` here and there cannot be one. Clerk has no
 * server-side password endpoint, so Wasp cannot post credentials on the app's
 * behalf -- the browser must talk to Clerk's Frontend API directly. Clerk's own
 * component does that.
 *
 * Compare `../../../wasp-auth/src/auth/LoginPage.tsx` and
 * `../../../better-auth/src/auth/LoginPage.tsx`. The login pages differ per
 * provider. Everything else in these apps does not.
 */
export function LoginPage() {
  return (
    <main style={{ display: "grid", placeItems: "center", marginTop: "3rem" }}>
      <SignIn />
    </main>
  );
}
