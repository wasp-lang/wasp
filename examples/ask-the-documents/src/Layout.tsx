import {
  Button,
  HeroUIProvider,
  Link,
  Navbar,
  NavbarBrand,
  NavbarContent,
  NavbarItem,
} from "@heroui/react";
import { Outlet } from "react-router-dom";
import {
  logout,
  googleSignInUrl as signInUrl,
  useAuth,
} from "wasp/client/auth";
import "./Main.css";

export function Layout() {
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
    <HeroUIProvider>
      <div className="text-foreground bg-background min-h-screen">
        <Navbar>
          <NavbarBrand>
            <p className="font-bold text-inherit">Ask The Documents</p>
          </NavbarBrand>
          <NavbarContent justify="end">
            <NavbarItem>{user ? logoutButton : loginButton}</NavbarItem>
          </NavbarContent>
        </Navbar>
        <Outlet />
      </div>
    </HeroUIProvider>
  );
}
