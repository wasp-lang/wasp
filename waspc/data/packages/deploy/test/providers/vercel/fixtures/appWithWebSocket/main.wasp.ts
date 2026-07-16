import { app } from "@wasp.sh/spec";

import { votingWebSocket } from "./src/ws-server";

export default app({
  name: "AppWithWebSocket",
  wasp: { version: "0.25.0" },
  webSocket: {
    fn: votingWebSocket,
  },
  spec: [],
});
