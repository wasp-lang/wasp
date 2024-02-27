import React from "react";

import logo from "./waspello-logo-navbar.svg";
import "./Navbar.css";
import { getName } from "./user";
import { logout } from "wasp/client/auth";

const Navbar = ({ user }) => {
  const name = getName(user);

  return (
    <div className="navbar">
      <div className="navbar-item">
        <span>Home</span>
      </div>

      <img alt="Waspello" className="navbar-logo navbar-item" src={logo} />

      <div className="navbar-item">
        <span>
          {name}
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
