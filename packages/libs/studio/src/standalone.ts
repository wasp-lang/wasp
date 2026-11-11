import { parseArgs } from "node:util";

import { createDevServer } from "devframe/adapters/dev";

import { createWaspStudioDevframe } from "./devframe";

// Serves Wasp Studio on its own, for `wasp studio`.
const { values } = parseArgs({
  options: {
    "data-file": { type: "string" },
    port: { type: "string", default: "4000" },
  },
});

const dataFile = values["data-file"];
if (!dataFile) {
  throw new Error("Missing the --data-file option.");
}

await createDevServer(createWaspStudioDevframe({ dataFile }), {
  port: Number(values.port),
  // Only reachable from this machine, like the rest of the dev tooling.
  auth: false,
  openBrowser: false,
});
