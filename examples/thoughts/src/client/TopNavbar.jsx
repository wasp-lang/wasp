import React from "react";

import logout from "@wasp/auth/logout";

import "./TopNavbar.css";

import { getUsername } from "@wasp/auth/user";

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
