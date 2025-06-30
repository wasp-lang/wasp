import { logout, useAuth } from "wasp/client/auth";
import { Link } from "wasp/client/router";
import Logo from "../../assets/logo.svg";
import { Button, ButtonLink } from "./Button";

export function Header() {
  const { data: user } = useAuth();

  return (
    <header className="sticky top-0 z-10 flex justify-center border-b border-neutral-200 bg-white shadow">
      <div className="flex w-full max-w-screen-lg items-center justify-between p-4 px-12">
        <Link to="/" className="flex items-center gap-2">
          <img src={Logo} alt="Todo App Logo" className="h-10 w-10" />
          <h1 className="text-2xl font-semibold">Todo App</h1>
        </Link>
        <nav>
          <ul className="flex gap-4 font-semibold">
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
                  <ButtonLink to="/login" variant="ghost">
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
