import { Outlet } from "react-router";
import "./App.css";
import { Header } from "./shared/components/Header";

export function App() {
  return (
    <div className="page">
      <Header />
      <Outlet />
    </div>
  );
}
