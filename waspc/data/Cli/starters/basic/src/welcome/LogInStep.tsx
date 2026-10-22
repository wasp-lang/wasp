import { twJoin } from "tailwind-merge";
import { type AuthUser } from "wasp/auth";
import { useAuth } from "wasp/client/auth";
import { Link } from "wasp/client/router";
import { primaryButtonClasses } from "./components/buttonStyles";
import { InlineCode } from "./components/InlineCode";
import { SampleUserButton } from "./sampleUser/SampleUserButton";

export function LogInStep() {
  const { data: user } = useAuth();

  return (
    <>
      <AuthStatus user={user ?? null} />
      <p>
        The <InlineCode>auth</InlineCode> field in{" "}
        <InlineCode>main.wasp.ts</InlineCode> sets up email and password login.
        Wasp takes care of the rest: storing users, sessions, and email
        verification. The pages in <InlineCode>src/auth</InlineCode> use our
        built-in Auth UI to let users sign up and log in.
      </p>
      <p>
        Sign up to try the whole flow, or let us automatically create a sample
        user in one click.
      </p>
      <div className="flex flex-wrap items-center gap-2">
        <Link to="/signup" className={primaryButtonClasses}>
          Sign up
        </Link>
        <SampleUserButton />
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
