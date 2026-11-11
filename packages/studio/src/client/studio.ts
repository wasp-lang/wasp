import { connectDevframe, type DevframeRpcClient } from "devframe/client";
import { useEffect, useState } from "react";

import { STUDIO_NAMESPACE } from "../shared/studio.js";
import type { Data } from "../shared/types.js";

type ConnectionStatus = DevframeRpcClient["status"];

export function useStudio() {
  const [status, setStatus] = useState<ConnectionStatus>("connecting");
  const [data, setData] = useState<Data | null>(null);

  useEffect(() => {
    let cleanup: (() => void) | undefined;
    let cancelled = false;

    void (async () => {
      const rpc = await connectDevframe();
      if (cancelled) return;
      setStatus(rpc.status);
      const offStatus = rpc.events.on("connection:status", setStatus);

      const dataState = await rpc
        .scope(STUDIO_NAMESPACE)
        .rpc.sharedState("data", { initialValue: { data: null } });
      if (cancelled) return offStatus();
      // Shared state is typed as deeply readonly. We never mutate it, so we
      // read it as plain `Data`.
      const readData = () => dataState.value().data as Data | null;
      setData(readData());
      const offData = dataState.on("updated", () => setData(readData()));

      cleanup = () => {
        offStatus();
        offData();
      };
    })();

    return () => {
      cancelled = true;
      cleanup?.();
    };
  }, []);

  return { status, data };
}
