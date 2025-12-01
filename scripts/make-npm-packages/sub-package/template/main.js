import { fileURLToPath } from "node:url";

export const waspBinPath = fileURLToPath(
  new URL("./wasp-bin", import.meta.url),
);

export const dataDirPath = fileURLToPath(new URL("./data/", import.meta.url));
