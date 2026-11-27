// @ts-check

import { join } from "node:path";

export default /** @type {import("wasp-cli-package-data-types").CliSubPackageAPI} */
({
  waspBinPath: join(import.meta.dirname, "./wasp-bin"),
  dataDirPath: join(import.meta.dirname, "./data/"),
});
