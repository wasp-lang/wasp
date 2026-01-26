import { Navbar, NavbarBrand, NavbarContent, NavbarItem } from "@heroui/react";
import { lazy, Suspense } from "react";
import "reactflow/dist/style.css";

import { Logo } from "./Logo";
import { useSocket } from "./socket";

const Flow = lazy(() => import("./Flow"));

export default function App() {
  const { data, isConnected } = useSocket();

  return (
    <div className="h-full">
      <Navbar position="static">
        <NavbarBrand>
          <Logo className="h-8 w-8" />
          <p className="ml-4 font-bold text-inherit">{data?.app.name}</p>
        </NavbarBrand>
        <NavbarContent justify="end">
          <NavbarItem>
            <div className="w-35 p-2 text-sm">
              {isConnected ? (
                <span className="flex items-center">
                  Connected
                  <span className="ml-2 inline-block h-2 w-2 rounded-full bg-green-500"></span>
                </span>
              ) : (
                <span className="flex items-center">
                  Connecting
                  <span className="ml-2 inline-block h-2 w-2 rounded-full bg-yellow-500"></span>
                </span>
              )}
            </div>
          </NavbarItem>
        </NavbarContent>
      </Navbar>
      <div className="flow-container">
        <Suspense fallback={<Loading />}>
          {data ? <Flow data={data} /> : <Loading />}
        </Suspense>
      </div>
    </div>
  );
}

function Loading() {
  return (
    <div className="flex h-full items-center justify-center">
      <p className="text-2xl text-gray-500">Loading...</p>
    </div>
  );
}
