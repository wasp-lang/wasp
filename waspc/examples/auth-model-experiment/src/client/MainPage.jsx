import waspLogo from "./waspLogo.png";
import "./Main.css";

import logout from "@wasp/auth/logout";
import useAuth from "@wasp/auth/useAuth";

import { Link } from "@wasp/router";

const MainPage = () => {
  const { data: user } = useAuth();
  return (
    <div className="container">
      <main>
        <div className="logo">
          <img src={waspLogo} alt="wasp" />
        </div>

        <h2 className="welcome-title">
          {" "}
          Welcome to Wasp - you just started a new app!{" "}
        </h2>

        {user && (
          <div className="user-info">
            <p>
              {" "}
              You are logged in as <strong>{user.auth.username}</strong>!{" "}
            </p>
            <p>
              {" "}
              Your user id is <strong>{user.id}</strong>.{" "}
            </p>
          </div>
        )}

        {user && (
          <button className="button" onClick={logout}>
            Logout
          </button>
        )}

        <Link to="/login" className="button">
          Login
        </Link>
      </main>
    </div>
  );
};
export default MainPage;
