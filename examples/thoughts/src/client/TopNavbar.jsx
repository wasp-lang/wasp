import { useEffect, useState } from "react";
import { api } from "wasp/client/api";
import { logout } from "wasp/client/auth";
import "./TopNavbar.css";

const TopNavbar = ({ user }) => {
  const [time, setTime] = useState(null);
  const username = user.getFirstProviderUserId();

  useEffect(() => {
    const getAndSetTime = async () => {
      const res = await api.get('/time');
      const { data: { time }} = res;

      setTime(time);
    }
    getAndSetTime();
  }, [])
  
  return (
    <div className="top-navbar">
      {username} @ {time}
      &nbsp;|&nbsp;
      <button className="plain" onClick={logout}>
        {" "}
        logout
        {" "}
      </button>
    </div>
  );
};

export default TopNavbar;
