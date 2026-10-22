import { logout, useAuth } from "wasp/client/auth";
import { Link } from "wasp/client/router";
import Logo from "../../assets/wasp-logo-rounded.svg";
import { Button, ButtonLink } from "./Button";

export function Header() {
  const { data: user } = useAuth();

  return (
    <header className="sticky top-0 z-10 flex items-center justify-between border-b-2 border-black bg-white px-6 py-3">
      <Link to="/">
        <h1 className="flex items-center gap-2 text-xl font-bold">
          <img src={Logo} alt="Wasp logo" className="h-10" />
          ToDo App
        </h1>
      </Link>
      <nav>
        <ul className="flex items-center gap-2">
          {user ? (
            <>
              <li>
                <ButtonLink to="/tasks" variant="ghost" size="sm">
                  My tasks
                </ButtonLink>
              </li>
              <li>
                <Button onClick={logout} size="sm">
                  Log out
                </Button>
              </li>
            </>
          ) : (
            <>
              <li>
                <ButtonLink to="/signup" size="sm">
                  Sign up
                </ButtonLink>
              </li>
              <li>
                <ButtonLink to="/login" variant="ghost" size="sm">
                  Login
                </ButtonLink>
              </li>
            </>
          )}
        </ul>
      </nav>
    </header>
  );
}
