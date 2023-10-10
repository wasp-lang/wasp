import { lazy, Suspense } from "react";
import {
  Navbar,
  NavbarBrand,
  NavbarContent,
  NavbarItem,
} from "@nextui-org/react";
import "reactflow/dist/style.css";

import { useSocket } from "./socket";
import { Logo } from "./Logo";

const Flow = lazy(() => import("./Flow"));

export default function App() {
  const { data, isConnected } = useSocket();

  return (
    <div className="h-full">
      <Navbar position="static">
        <NavbarBrand>
          <Logo className="w-8 h-8" />
          <p className="font-bold text-inherit ml-4">{data?.app.name}</p>
        </NavbarBrand>
        <NavbarContent justify="end">
          <NavbarItem>
            <div className="text-sm p-2 w-35">
              {isConnected ? (
                /* Green dot */ <span className="flex items-center">
                  Connected
                  <span className="w-2 h-2 bg-green-500 rounded-full inline-block ml-2"></span>
                </span>
              ) : (
                /* Red dot */ <span className="flex items-center">
                  Connecting
                  <span className="w-2 h-2 bg-yellow-500 rounded-full inline-block ml-2"></span>
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
    <div className="flex items-center justify-center h-full">
      <p className="text-2xl text-gray-500">Loading...</p>
    </div>
  );
}
