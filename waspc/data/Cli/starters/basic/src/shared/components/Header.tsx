import { logout, useAuth } from "wasp/client/auth";
import { Link } from "wasp/client/router";
import Logo from "../../assets/wasp-logo-rounded.svg";
import { Button, ButtonLink } from "./Button";

export function Header() {
  const { data: user } = useAuth();

  return (
    <header className="sticky top-0 z-10">
      <Link to="/">
        <h1 className="font-bold">
          <img src={Logo} alt="Wasp logo" />
          To-Do App
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
