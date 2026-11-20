import { resolve } from "node:path";
import { parseArgs } from "node:util";

import express from "express";
import morgan from "morgan";

const { values: options } = parseArgs({
  options: { "data-file": { type: "string" } },
});
const dataFile = options["data-file"];
if (!dataFile) {
  throw new Error("Missing the --data-file <path> option.");
}
const dataFilePath = resolve(dataFile);

const app = express();

// The app's spec, which `wasp studio` writes to the data file. The Wasp Studio
// page reads it from the same route as on the server that `wasp start` runs.
// Registered before the request logger, since the page polls it.
app.get("/__wasp/spec", (_req, res) => {
  // The data file is in the `.wasp` dir, which `sendFile` refuses by default.
  res.type("json").sendFile(dataFilePath, { dotfiles: "allow" });
});

app.use(morgan("dev"));

const publicDirPath = new URL("./public", import.meta.url).pathname;
app.use(express.static(publicDirPath));

app.listen(4000, () => {
  console.log("Server listening on port 4000");
});
