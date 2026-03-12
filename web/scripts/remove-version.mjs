// @ts-check

import assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { parseArgs } from "node:util";

const DOCUSAURUS_ROOT = path.resolve(import.meta.dirname, "..");

const { positionals } = parseArgs({
  options: {},
  strict: true,
  allowPositionals: true,
});
assert(
  positionals.length === 1,
  "Expected exactly one positional argument: the version to remove",
);
const [VERSION] = positionals;

const VERSIONED_DOCS_DIR = path.join(
  DOCUSAURUS_ROOT,
  `versioned_docs/version-${VERSION}`,
);
const VERSIONED_SIDEBARS_FILE = path.join(
  DOCUSAURUS_ROOT,
  `versioned_sidebars/version-${VERSION}-sidebars.json`,
);
const VERSIONS_FILE = path.join(DOCUSAURUS_ROOT, "versions.json");

await fs.rm(VERSIONED_DOCS_DIR, { recursive: true, force: true });
await fs.rm(VERSIONED_SIDEBARS_FILE);

const oldVersions = /** @type {string[]} */ (
  JSON.parse(await fs.readFile(VERSIONS_FILE, "utf-8"))
);
const newVersions = oldVersions.filter((v) => v !== VERSION);
await fs.writeFile(VERSIONS_FILE, JSON.stringify(newVersions, null, 2) + "\n");
