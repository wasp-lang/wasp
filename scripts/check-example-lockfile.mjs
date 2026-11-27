import { globSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const libsDirectory = fileURLToPath(
  new URL("../packages/libs/", import.meta.url),
);
const { packages = {} } = JSON.parse(readFileSync("package-lock.json", "utf8"));

// Both generated workspaces depend on every bundled Wasp library.
const staleLibraries = globSync("*/package.json", { cwd: libsDirectory })
  .map(
    (manifest) =>
      JSON.parse(readFileSync(join(libsDirectory, manifest), "utf8")).name,
  )
  .filter(
    (name) =>
      !packages[`node_modules/${name}`]?.resolved?.startsWith(
        "file:.wasp/out/libs/",
      ),
  )
  .sort();

if (staleLibraries.length > 0) {
  console.error(
    `package-lock.json is missing local Wasp library entries for ${staleLibraries.join(", ")}. ` +
      "Run `mise run bust-libs-cache` from this example directory and commit the updated lockfile.",
  );
  process.exitCode = 1;
}
