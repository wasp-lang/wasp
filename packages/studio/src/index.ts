import { readFileSync, watch } from "node:fs";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { parseArgs } from "node:util";

import { defineDevframe } from "devframe";
import { createDevServer } from "devframe/adapters/dev";

import { STUDIO_NAMESPACE } from "./shared/studio.js";
import type { Data } from "./shared/types.js";

const { values: args } = parseArgs({
  options: { "data-file": { type: "string", short: "d" } },
});
if (!args["data-file"]) {
  throw new Error("Missing required option: --data-file <path>");
}
const dataFilePath = resolve(args["data-file"]);

// Paths are relative to the package root so they work both from `src/` (in
// development) and from `dist/`.
const packageRoot = new URL("../", import.meta.url);
const pkg = JSON.parse(
  readFileSync(new URL("package.json", packageRoot), "utf8"),
);

const studio = defineDevframe({
  id: STUDIO_NAMESPACE,
  name: "Wasp Studio",
  version: pkg.version,
  packageName: pkg.name,
  homepage: pkg.homepage,
  description: pkg.description,
  importMetaUrl: import.meta.url,
  clientAssets: fileURLToPath(new URL("dist/client", packageRoot)),
  cli: {
    port: 4000,
    portRange: [4000, 4100],
    // Studio only shows the app's structure and binds to localhost, so it
    // skips devframe's sign-in.
    auth: false,
  },
  async setup(ctx) {
    const dataState = await ctx
      .scope(STUDIO_NAMESPACE)
      .rpc.sharedState("data", { initialValue: { data: null } });

    async function loadData() {
      let data: Data;
      try {
        data = JSON.parse(await readFile(dataFilePath, "utf8"));
      } catch {
        // The file can be caught mid-write, so we keep the last good data and
        // wait for the next change.
        return;
      }
      dataState.mutate(() => ({ data }));
    }

    await loadData();
    watch(dataFilePath, () => void loadData());
  },
});

await createDevServer(studio, {
  onReady({ origin }) {
    console.log(`➜ Open in your browser: ${origin}`);
  },
});
