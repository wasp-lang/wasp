import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";

const validator = fileURLToPath(
  new URL("./check-example-lockfile.mjs", import.meta.url),
);

test("rejects stale Wasp library tarball paths", (t) => {
  const cwd = mkdtempSync(join(tmpdir(), "wasp-lockfile-"));
  t.after(() => rmSync(cwd, { recursive: true, force: true }));

  const lockfile = JSON.parse(
    readFileSync(
      new URL("../examples/kitchen-sink/package-lock.json", import.meta.url),
      "utf8",
    ),
  );
  const path = join(cwd, "package-lock.json");
  writeFileSync(path, JSON.stringify(lockfile));
  const valid = spawnSync(process.execPath, [validator], {
    cwd,
    encoding: "utf8",
  });
  assert.equal(valid.status, 0, valid.stderr);

  const sdkCore = lockfile.packages["node_modules/@wasp/lib-sdk-core"];
  const expected = sdkCore.resolved;
  const stale = expected.replace(/[^/]+\.tgz$/, "wasp-lib-sdk-core-0.0.0.tgz");
  sdkCore.resolved = stale;
  writeFileSync(path, JSON.stringify(lockfile));

  const invalid = spawnSync(process.execPath, [validator], {
    cwd,
    encoding: "utf8",
  });
  assert.equal(invalid.status, 1, invalid.stderr);
  assert.ok(invalid.stderr.includes(`expected ${expected}, found ${stale}`));
  assert.ok(invalid.stderr.includes("mise run bust-libs-cache"));
});
