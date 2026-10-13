import { twJoin } from "tailwind-merge";
import { type AuthUser } from "wasp/auth";
import { ButtonLink } from "../shared/components/Button";
import { InlineCode } from "./components/InlineCode";
import { SampleUserButton } from "./sampleUser/SampleUserButton";

export function LogInStep({ user }: { user: AuthUser | null }) {
  return (
    <>
      <AuthStatus user={user} />
      <p>
        This app uses email and password auth. In development, Wasp doesn&apos;t
        send real emails: the verification link is printed in the terminal where{" "}
        <InlineCode>wasp start</InlineCode> is running.
      </p>
      <div className="flex flex-wrap items-center gap-2">
        <ButtonLink to="/signup" size="sm">
          Sign up
        </ButtonLink>
        {/* Vite replaces `import.meta.env.DEV` at build time, so the button is
            dropped from production builds. */}
        {import.meta.env.DEV && <SampleUserButton />}
      </div>
    </>
  );
}

function AuthStatus({ user }: { user: AuthUser | null }) {
  return (
    <p className="flex items-center gap-2 font-mono text-xs uppercase">
      <span
        className={twJoin(
          "size-2 rounded-full",
          user ? "bg-green-500" : "bg-neutral-400",
        )}
      />
      {user ? `Logged in as ${user.username}` : "Not logged in"}
    </p>
  );
}
