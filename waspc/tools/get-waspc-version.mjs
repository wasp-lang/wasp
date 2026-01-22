// @ts-check

// Gets the waspc version from the Cabal package file.

import { readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const waspcDir = fileURLToPath(new URL("..", import.meta.url));
const cabalFile = readFileSync(join(waspcDir, "waspc.cabal"), "utf-8");

const match = cabalFile.match(/^version:\s*(.+)$/m);
if (!match) {
  throw new Error("Could not find version in waspc.cabal");
}

console.log(match[1].trim());
