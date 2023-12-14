import React from "react";

import logout from "@wasp/auth/logout";

import logo from "./waspello-logo-navbar.svg";
import "./Navbar.css";

const Navbar = ({ user }) => {
  const identifier = user.auth?.identities[0]?.providerUserId;
  return (
    <div className="navbar">
      <div className="navbar-item">
        <span>Home</span>
      </div>

      <img alt="Waspello" className="navbar-logo navbar-item" src={logo} />

      <div className="navbar-item">
        <span>
          {identifier}
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
