import { type User } from "wasp/entities";

import { logout } from "wasp/client/auth";
import "./Main.css";

import React from "react";

const MainPage = ({ user }: { user: User }) => {
  return (
    <div className="container">
      <main>
        {user ? JSON.stringify(user, null, 2) : null}
        <button onClick={logout}>Logout</button>
      </main>
    </div>
  );
};

export default MainPage;
