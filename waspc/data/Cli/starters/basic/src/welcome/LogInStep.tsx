import { twJoin } from "tailwind-merge";
import { type AuthUser } from "wasp/auth";
import { useAuth } from "wasp/client/auth";
import { Link } from "wasp/client/router";
import { InlineCode } from "./components/InlineCode";

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
      <p>You can create a sample user by going to the Sign Up page:</p>
      <div>
        <Link
          to="/signup"
          className="inline-flex items-center gap-2 border-2 border-black bg-yellow px-5 py-3 font-bold text-black hover:bg-yellow-dark"
        >
          Create your first user
        </Link>
      </div>
    </>
  );
}

function AuthStatus({ user }: { user: AuthUser | null }) {
  return (
    <p className="flex items-center gap-2 text-xs font-bold uppercase">
      <span
        className={twJoin(
          "size-2 rounded-full",
          user ? "bg-green-600" : "bg-gray",
        )}
      />
      {user ? `Logged in as ${user.username}` : "Not logged in"}
    </p>
  );
}
