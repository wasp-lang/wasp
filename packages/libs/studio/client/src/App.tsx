import "@xyflow/react/dist/style.css";
import { lazy, Suspense } from "react";

import { Logo } from "./Logo";
import { useStudioData } from "./studioData";

const Flow = lazy(() => import("./Flow"));

export default function App() {
  const { data, status } = useStudioData();

  return (
    <div className="h-full">
      <nav className="flex w-full items-center justify-center">
        <header className="flex h-16 w-full max-w-5xl items-center justify-between gap-4 px-6">
          <div className="flex items-center">
            <Logo className="h-8 w-8" />
            <p className="ml-4 font-bold">{data?.app.name}</p>
          </div>
          <div className="p-2 text-sm">
            {status === "connected" ? (
              <span className="flex items-center">
                Connected
                <span className="ml-2 inline-block h-2 w-2 rounded-full bg-green-500"></span>
              </span>
            ) : status === "unauthorized" ? (
              <span className="flex items-center">
                Open the link printed in your terminal
                <span className="ml-2 inline-block h-2 w-2 rounded-full bg-red-500"></span>
              </span>
            ) : (
              <span className="flex items-center">
                Connecting
                <span className="ml-2 inline-block h-2 w-2 rounded-full bg-yellow-500"></span>
              </span>
            )}
          </div>
        </header>
      </nav>
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
