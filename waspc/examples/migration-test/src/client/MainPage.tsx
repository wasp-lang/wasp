import "./Main.css";

import React from "react";
import logout from "@wasp/auth/logout";

import { User } from "@wasp/entities";

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
