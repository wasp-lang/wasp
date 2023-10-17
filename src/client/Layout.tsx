import {
  NextUIProvider,
  Navbar,
  NavbarBrand,
  NavbarContent,
  NavbarItem,
  Link,
  Button,
} from "@nextui-org/react";
import useAuth from "@wasp/auth/useAuth";
import { signInUrl } from "@wasp/auth/helpers/Google";
import logout from "@wasp/auth/logout";
import "./Main.css";

export function Layout({ children }: React.PropsWithChildren<{}>) {
  const { data: user } = useAuth();

  const loginButton = (
    <Button as={Link} color="primary" href={signInUrl} variant="flat">
      Login with Google
    </Button>
  );

  const logoutButton = (
    <Button color="danger" variant="flat" onClick={logout}>
      Logout
    </Button>
  );

  return (
    <NextUIProvider>
      <div className="dark text-foreground bg-background min-h-screen">
        <Navbar>
          <NavbarBrand>
            <p className="font-bold text-inherit">Ask The Documents</p>
          </NavbarBrand>
          <NavbarContent justify="end">
            <NavbarItem>{user ? logoutButton : loginButton}</NavbarItem>
          </NavbarContent>
        </Navbar>
        {children}
      </div>
    </NextUIProvider>
  );
}
