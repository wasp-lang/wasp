import React from "react";

import logout from "@wasp/auth/logout";

import logo from "./waspello-logo-navbar.svg";
import "./Navbar.css";

import { findUserIdentity, getUsername } from "@wasp/auth/user";

const Navbar = ({ user }) => {
  // We have two ways of authenticating users, so
  // we have to check which one is used.
  const googleIdentity = findUserIdentity(user, "google");
  const usernameIdentity = findUserIdentity(user, "username");

  const username = usernameIdentity
    ? getUsername(user)
    : `Google user ${googleIdentity.providerUserId}`;

  return (
    <div className="navbar">
      <div className="navbar-item">
        <span>Home</span>
      </div>

      <img alt="Waspello" className="navbar-logo navbar-item" src={logo} />

      <div className="navbar-item">
        <span>
          {username}
          &nbsp;|&nbsp;
          <button className="logout-btn" onClick={logout}>
            {" "}
            logout{" "}
          </button>
        </span>
      </div>
    </div>
  );
};

export default Navbar;
