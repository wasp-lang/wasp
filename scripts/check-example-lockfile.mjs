import { globSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const libsDirectory = fileURLToPath(
  new URL("../packages/libs/", import.meta.url),
);
const { packages = {} } = JSON.parse(readFileSync("package-lock.json", "utf8"));

// Both generated workspaces depend on every bundled Wasp library.
const staleLibraries = globSync("*/package.json", { cwd: libsDirectory })
  .map((manifest) => {
    const { name, version } = JSON.parse(
      readFileSync(join(libsDirectory, manifest), "utf8"),
    );
    // Match the npm tarball naming convention used by the Wasp generator.
    const tarballName = name.replace(/^@/, "").replaceAll("/", "-");
    const expected = `file:.wasp/out/libs/${dirname(manifest)}/${tarballName}-${version}.tgz`;
    const resolved = packages[`node_modules/${name}`]?.resolved;
    return { name, expected, resolved };
  })
  .filter(({ expected, resolved }) => resolved !== expected)
  .map(
    ({ name, expected, resolved }) =>
      `  ${name}: expected ${expected}, found ${resolved ?? "(missing)"}`,
  )
  .sort();

if (staleLibraries.length > 0) {
  console.error(
    `package-lock.json has missing or stale Wasp library entries:\n${staleLibraries.join("\n")}\n` +
      "Run `mise run bust-libs-cache` from this example directory and commit the updated lockfile.",
  );
  process.exitCode = 1;
}
