import { useState, useEffect, lazy, Suspense } from "react";
import {
  Navbar,
  NavbarBrand,
  NavbarContent,
  NavbarItem,
  Link,
  Button,
} from "@nextui-org/react";
import "reactflow/dist/style.css";

import logo from "./assets/logo.png";

import { socket } from "./socket";
import { Data } from "./types";
// import { Flow } from "./Flow";

const Flow = lazy(() => import("./Flow"));

export default function App() {
  const [data, setData] = useState<Data | null>(null);

  function onData(data: string) {
    console.log("data", data);
    setData(JSON.parse(data) as Data);
  }

  useEffect(() => {
    socket.on("data", onData);
    return () => {
      socket.off("data", onData);
    };
  }, []);

  return (
    <div className="h-full">
      <Navbar position="static">
        <NavbarBrand>
          <img src={logo} alt="logo" className="w-8 h-8" />
          <p className="font-bold text-inherit ml-4">{data?.app.name}</p>
        </NavbarBrand>
        {/* <NavbarContent className="hidden sm:flex gap-4" justify="center">
          <NavbarItem>
            <Link color="foreground" href="#">
              Features
            </Link>
          </NavbarItem>
          <NavbarItem isActive>
            <Link href="#" aria-current="page">
              Customers
            </Link>
          </NavbarItem>
          <NavbarItem>
            <Link color="foreground" href="#">
              Integrations
            </Link>
          </NavbarItem>
        </NavbarContent> */}
        <NavbarContent justify="end">
          {/* <NavbarItem className="hidden lg:flex">
            <Link href="#">Login</Link>
          </NavbarItem> */}
          <NavbarItem>
            <Button as={Link} color="primary" href="#" variant="flat">
              Close the Studio
            </Button>
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
