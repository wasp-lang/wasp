import { connectDevframe } from "devframe/client";
import { useEffect, useState } from "react";

import type { Data } from "./types";

const STUDIO_ID = "wasp-studio";

type StudioState = { data: Data | null };

export function useStudioData() {
  const [data, setData] = useState<Data | null>(null);
  const [isConnected, setIsConnected] = useState(false);

  useEffect(() => {
    let cleanup = () => {};
    let isCancelled = false;

    (async () => {
      const client = await connectDevframe();
      // Until the client is trusted, this resolves to the initial value, and
      // the server's value arrives later as an update.
      const state = await client
        .scope(STUDIO_ID)
        .rpc.sharedState<StudioState>("state", {
          initialValue: { data: null },
        });
      if (isCancelled) {
        return;
      }

      setIsConnected(client.status === "connected");
      setData(state.value().data as Data | null);

      const offStatus = client.events.on("connection:status", (status) =>
        setIsConnected(status === "connected"),
      );
      const offState = state.on("updated", (fullState) =>
        setData(fullState.data),
      );
      cleanup = () => {
        offStatus();
        offState();
      };
    })().catch(console.error);

    return () => {
      isCancelled = true;
      cleanup();
    };
  }, []);

  return { data, isConnected };
}
