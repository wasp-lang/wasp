import { connectDevframe, type DevframeRpcClient } from "devframe/client";
import { useSyncExternalStore } from "react";

import { STUDIO_NAMESPACE } from "../shared/studio.js";
import type { Data } from "../shared/types.js";

type ConnectionStatus = DevframeRpcClient["status"];

interface StudioSnapshot {
  status: ConnectionStatus;
  data: Data | null;
}

let snapshot: StudioSnapshot = { status: "connecting", data: null };

function getSnapshot(): StudioSnapshot {
  return snapshot;
}

function subscribe(onStoreChange: () => void): () => void {
  const controller = new AbortController();
  const { signal } = controller;
  // Runs `cleanup` once React unsubscribes.
  const onAbort = (cleanup: () => void) =>
    signal.addEventListener("abort", cleanup, { once: true });
  const update = (changes: Partial<StudioSnapshot>) => {
    snapshot = { ...snapshot, ...changes };
    onStoreChange();
  };

  void (async () => {
    const rpc = await connectDevframe();
    if (signal.aborted) return;
    update({ status: rpc.status });
    onAbort(rpc.events.on("connection:status", (status) => update({ status })));

    const dataState = await rpc
      .scope(STUDIO_NAMESPACE)
      .rpc.sharedState("data", { initialValue: { data: null } });
    if (signal.aborted) return;
    // Shared state is typed as deeply readonly. We never mutate it, so we
    // read it as plain `Data`.
    const readData = () => dataState.value().data as Data | null;
    update({ data: readData() });
    onAbort(dataState.on("updated", () => update({ data: readData() })));
  })();

  return () => controller.abort();
}

export function useStudio(): StudioSnapshot {
  return useSyncExternalStore(subscribe, getSnapshot);
}
