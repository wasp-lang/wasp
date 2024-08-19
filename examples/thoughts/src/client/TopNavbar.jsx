import { logout } from "wasp/client/auth";

import React from "react";

import "./TopNavbar.css";

const TopNavbar = ({ user }) => {
  const username = user.getFirstProviderUserId();

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
