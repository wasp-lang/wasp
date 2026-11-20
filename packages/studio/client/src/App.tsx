import "@xyflow/react/dist/style.css";
import { lazy, Suspense } from "react";

import { Loading } from "./Loading";
import { Logo } from "./Logo";
import { useSocket } from "./socket";

const Flow = lazy(() => import("./Flow"));

export default function App() {
  const { data, isConnected } = useSocket();

  return (
    <div className="h-full">
      <nav className="border-wasp-g3 bg-wasp-bg border-b">
        <header className="flex h-16 w-full items-center justify-between gap-4 px-6">
          <div className="flex items-center gap-3">
            <Logo className="border-wasp-black h-9 w-9 border-2" />
            <span className="font-mono text-xl font-extrabold tracking-tight">
              wasp
            </span>
            <span className="bg-wasp-yellow px-2 py-0.5 font-mono text-[10px] font-bold tracking-widest uppercase">
              Studio
            </span>
            {data?.app.name && (
              <>
                <span className="text-wasp-g4 font-mono">/</span>
                <span className="font-mono text-sm font-bold">
                  {data.app.name}
                </span>
              </>
            )}
          </div>
          <div className="text-wasp-g6 flex items-center gap-2 font-mono text-xs font-bold tracking-widest uppercase">
            <span
              className={`border-wasp-black inline-block h-2 w-2 border ${isConnected ? "bg-green-500" : "bg-wasp-yellow"}`}
            />
            {isConnected ? "Connected" : "Connecting"}
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
