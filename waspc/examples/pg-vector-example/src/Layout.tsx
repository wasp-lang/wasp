import { NextUIProvider } from "@nextui-org/react";
import "./Main.css";
import { Outlet } from "react-router-dom";

export function Layout() {
  return (
    <NextUIProvider>
      <div className="dark text-foreground bg-background min-h-screen">
        <Outlet />
      </div>
    </NextUIProvider>
  );
}
