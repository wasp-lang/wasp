import React from "react";

import logout from "@wasp/auth/logout";

import "./TopNavbar.css";

function getUsername(user) {
  const usernameIdentity = user.auth.identities.find(
    (identity) => identity.providerName === "username"
  );
  return usernameIdentity ? usernameIdentity.providerUserId : null;
}

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
