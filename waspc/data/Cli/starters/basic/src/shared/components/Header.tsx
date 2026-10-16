import { logout, useAuth } from "wasp/client/auth";
import { Link } from "wasp/client/router";
import Logo from "../../assets/wasp-logo-rounded.svg";
import { Button, ButtonLink } from "./Button";

export function Header() {
  const { data: user } = useAuth();

  return (
    <header className="sticky top-0 z-10 flex justify-center border-b border-neutral-900 bg-white">
      <div className="flex w-full max-w-(--breakpoint-lg) items-center justify-between px-6 py-4 lg:px-12">
        <Link to="/" className="flex items-center gap-2">
          <img src={Logo} alt="Wasp logo" className="h-10 w-10" />
          <h1 className="font-mono text-xl font-extrabold tracking-tight">
            ToDo App
          </h1>
        </Link>
        <nav>
          <ul className="flex items-center gap-3">
            {user ? (
              <li>
                <Button onClick={logout}>Log out</Button>
              </li>
            ) : (
              <>
                <li>
                  <ButtonLink to="/signup">Sign up</ButtonLink>
                </li>
                <li>
                  <ButtonLink to="/login" variant="secondary">
                    Login
                  </ButtonLink>
                </li>
              </>
            )}
          </ul>
        </nav>
      </div>
    </header>
  );
}
