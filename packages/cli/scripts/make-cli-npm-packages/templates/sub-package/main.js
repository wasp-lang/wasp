// @ts-check

import { join } from "node:path";

export default /** @type {import("../../src/schema/output-data.ts").CliSubPackageAPI} */
({
  waspBinPath: join(import.meta.dirname, "./wasp-bin"),
  dataDirPath: join(import.meta.dirname, "./data/"),
});
