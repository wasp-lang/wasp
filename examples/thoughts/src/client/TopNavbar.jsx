import { getUsername } from "wasp/auth";

import { logout } from "wasp/client/auth";

import React from "react";

import "./TopNavbar.css";

const TopNavbar = ({ user }) => {
  const username = getUsername(user);

  return (
    <div className="top-navbar">
      {username}
      &nbsp;|&nbsp;
      <button className="plain" onClick={logout}>
        {" "}
        logout{" "}
      </button>
    </div>
  );
};

export default TopNavbar;
