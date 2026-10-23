// Copies the server bundle and only the files it needs at runtime into
// `production/`, so the final Docker image doesn't need the full `node_modules`.
//
// We trace:
// - `bundle/server.js`: the bundled server. Its externals are what we need.
// - `dotenv/config`: preloaded with `node -r dotenv/config` in the `start` script.
// - The Prisma CLI, but only when `start-production` runs migrations.
import { traceNodeModules } from "nf3";
import {
  cpSync,
  globSync,
  mkdirSync,
  readFileSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";

/** @typedef {import("nf3").TracedLayout} TracedLayout */

/**
 * @typedef {object} ServerPackageJson
 * @property {string} name
 * @property {Record<string, string>} scripts
 */

const serverDir = import.meta.dirname;
const outDir = path.join(serverDir, "production");
const requireFromServer = createRequire(path.join(serverDir, "package.json"));

/** @type {ServerPackageJson} */
const serverPackageJson = JSON.parse(
  readFileSync(path.join(serverDir, "package.json"), "utf8"),
);
const runsMigrations =
  serverPackageJson.scripts["start-production"].includes("db-migrate-prod");
const prismaCliEntry = runsMigrations
  ? requireFromServer.resolve("prisma/build/index.js")
  : undefined;

rmSync(outDir, { recursive: true, force: true });
mkdirSync(outDir, { recursive: true });

const layout = await traceNodeModules(
  [
    path.join(serverDir, "bundle/server.js"),
    requireFromServer.resolve("dotenv/config"),
    ...(prismaCliEntry ? [prismaCliEntry] : []),
  ],
  { rootDir: serverDir, outDir, chmod: true },
);
if (prismaCliEntry) {
  copyPrismaCliEngines(layout);
}
cpSync(path.join(serverDir, "bundle"), path.join(outDir, "bundle"), {
  recursive: true,
});
writeProductionPackageJson(layout);

console.log(
  `Traced ${Object.keys(layout.packages).length} packages into ${outDir}`,
);

/**
 * The tracer misses the Prisma CLI's engine binaries (the schema engine and
 * the CLI's own copy of the query engine), because the CLI finds them by path
 * at runtime instead of importing them. Without them, the CLI downloads them
 * on every container start, or crashes when there's no internet access.
 *
 * @param {TracedLayout} layout
 * @see https://github.com/unjs/nf3#readme
 */
function copyPrismaCliEngines(layout) {
  const prismaPkgDir = realpathSync(
    path.dirname(requireFromServer.resolve("prisma/package.json")),
  );
  const enginesPkgDir = realpathSync(
    path.dirname(
      createRequire(path.join(prismaPkgDir, "package.json")).resolve(
        "@prisma/engines/package.json",
      ),
    ),
  );
  const enginesOutDir = path.join(
    outDir,
    getHoistedOutPath(layout, "@prisma/engines") ??
      "node_modules/@prisma/engines",
  );

  mkdirSync(enginesOutDir, { recursive: true });
  for (const engine of globSync(["schema-engine-*", "libquery_engine-*"], {
    cwd: enginesPkgDir,
  })) {
    cpSync(path.join(enginesPkgDir, engine), path.join(enginesOutDir, engine));
  }
}

/**
 * `npm run` puts `node_modules/.bin` on the PATH, but the traced output has
 * no `.bin` folder, so we call the Prisma CLI through its entry file instead.
 *
 * @param {TracedLayout} layout
 */
function writeProductionPackageJson(layout) {
  const { scripts } = serverPackageJson;
  /** @type {Record<string, string>} */
  const productionScripts = {
    start: scripts.start,
    "start-production": scripts["start-production"],
  };
  if (prismaCliEntry) {
    const prismaCliInOutDir =
      layout.files[prismaCliEntry] ?? path.relative(serverDir, prismaCliEntry);
    productionScripts["db-migrate-prod"] = scripts["db-migrate-prod"].replace(
      /^prisma /,
      `node ${prismaCliInOutDir} `,
    );
  }

  writeFileSync(
    path.join(outDir, "package.json"),
    JSON.stringify(
      {
        name: serverPackageJson.name,
        private: true,
        type: "module",
        scripts: productionScripts,
      },
      null,
      2,
    ),
  );
}

/**
 * @param {TracedLayout} layout
 * @param {string} packageName
 * @returns {string | undefined} The package's path in `outDir`, if traced.
 */
function getHoistedOutPath(layout, packageName) {
  const versions = layout.packages[packageName]?.versions ?? {};
  return Object.values(versions).find((version) => version.hoisted)?.outPath;
}
