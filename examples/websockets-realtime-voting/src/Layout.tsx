import { Outlet } from "react-router-dom";
import { logout, useAuth } from "wasp/client/auth";
import "./Main.css";

import {
  Avatar,
  Dropdown,
  DropdownDivider,
  DropdownHeader,
  DropdownItem,
  Navbar,
  NavbarBrand,
  NavbarToggle,
  ThemeProvider,
} from "flowbite-react";
import Logo from "./logo.png";

const customTheme = {
  button: {
    color: {
      primary: "bg-red-500 hover:bg-red-600",
    },
  },
};

export const Layout = () => {
  const { data: user } = useAuth();

  return (
    <ThemeProvider theme={customTheme}>
      <div className="p-8">
        <Navbar fluid rounded>
          <NavbarBrand className="cursor-pointer">
            <img alt="Fox Logo" className="mr-3 h-6 sm:h-9" src={Logo} />
            <span className="self-center whitespace-nowrap text-xl font-semibold dark:text-white">
              Undecisive Fox App
            </span>
          </NavbarBrand>
          {user && (
            <div className="user-dropdown flex md:order-2">
              <Dropdown
                inline
                label={
                  <Avatar
                    alt="User settings"
                    img={`https://xsgames.co/randomusers/avatar.php?g=female&username=${user.getFirstProviderUserId()}`}
                    rounded
                  />
                }
              >
                <DropdownHeader>
                  <span className="block text-sm">
                    {user.getFirstProviderUserId()}
                  </span>
                </DropdownHeader>
                <DropdownItem>Dashboard</DropdownItem>
                <DropdownItem>Settings</DropdownItem>
                <DropdownItem>Earnings</DropdownItem>
                <DropdownDivider />
                <DropdownItem onClick={logout}>Sign out</DropdownItem>
              </Dropdown>
              <NavbarToggle />
            </div>
          )}
          {/* <Navbar.Collapse>
            <Navbar.Link active href="#">
              <p>Home</p>
            </Navbar.Link>
            <Navbar.Link href="#">About</Navbar.Link>
            <Navbar.Link href="#">Services</Navbar.Link>
            <Navbar.Link href="#">Pricing</Navbar.Link>
            <Navbar.Link href="#">Contact</Navbar.Link>
          </Navbar.Collapse> */}
        </Navbar>
        <div className="mt-8 grid place-items-center">
          <Outlet />
        </div>
      </div>
    </ThemeProvider>
  );
};
