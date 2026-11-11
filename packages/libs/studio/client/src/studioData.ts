import { connectDevframe, type DevframeRpcClient } from "devframe/client";
import { useSyncExternalStore } from "react";

import type { Data } from "./types";

const STUDIO_ID = "wasp-studio";

interface StudioSnapshot {
  status: DevframeRpcClient["status"];
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
    const client = await connectDevframe();
    if (signal.aborted) return;
    update({ status: client.status });
    onAbort(
      client.events.on("connection:status", (status) => update({ status })),
    );

    // Until the client is trusted, this resolves to the initial value, and
    // the server's value arrives later as an update.
    const state = await client
      .scope(STUDIO_ID)
      .rpc.sharedState<{ data: Data | null }>("state", {
        initialValue: { data: null },
      });
    if (signal.aborted) return;
    // Shared state is typed as deeply readonly. We never mutate it, so we
    // read it as plain `Data`.
    const readData = () => state.value().data as Data | null;
    update({ data: readData() });
    onAbort(state.on("updated", () => update({ data: readData() })));
  })();

  return () => controller.abort();
}

export function useStudioData(): StudioSnapshot {
  return useSyncExternalStore(subscribe, getSnapshot);
}
