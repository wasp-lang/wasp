import "@xyflow/react/dist/style.css";
import type { DevframeRpcClient } from "devframe/client";
import { lazy, Suspense } from "react";

import { Logo } from "./Logo";
import { useStudio } from "./studio";

const Flow = lazy(() => import("./Flow"));

export default function App() {
  const { status, data } = useStudio();

  return (
    <div className="h-full">
      <nav className="flex w-full items-center justify-center">
        <header className="flex h-16 w-full max-w-5xl items-center justify-between gap-4 px-6">
          <div className="flex items-center">
            <Logo className="h-8 w-8" />
            <p className="ml-4 font-bold">{data?.app.name}</p>
          </div>
          <div className="w-35 p-2 text-sm">
            <ConnectionStatus status={status} />
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

function ConnectionStatus({ status }: { status: DevframeRpcClient["status"] }) {
  const [label, color] =
    status === "connected"
      ? ["Connected", "bg-green-500"]
      : status === "connecting"
        ? ["Connecting", "bg-yellow-500"]
        : status === "unauthorized"
          ? ["Not signed in", "bg-yellow-500"]
          : ["Disconnected", "bg-red-500"];

  return (
    <span className="flex items-center">
      {label}
      <span className={`ml-2 inline-block h-2 w-2 rounded-full ${color}`} />
    </span>
  );
}

function Loading() {
  return (
    <div className="flex h-full items-center justify-center">
      <p className="text-2xl text-gray-500">Loading...</p>
    </div>
  );
}
