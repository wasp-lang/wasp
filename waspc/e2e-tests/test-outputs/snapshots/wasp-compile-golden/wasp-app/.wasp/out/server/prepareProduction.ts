/*
  Copies the server bundle and only the files it needs at runtime into
  `production/`, so the final Docker image doesn't need the full `node_modules`.

  We trace:
  - `bundle/server.js`: the bundled server. Rollup leaves the packages it
    imports out of the bundle, so following those imports finds the files the
    server needs at runtime.
  - `dotenv/config`: preloaded with `node -r dotenv/config` in the `start` script.
  - The Prisma CLI, copied in full, but only when `start-production` runs
    migrations.
  - The packages the user lists in `app.server.traceDeps`.

  TODO: Research whether we can keep the Prisma CLI out of the production image.
*/
import { traceNodeModules } from "nf3";
import { FullTracePackages } from "nf3/db";
import { execFile } from "node:child_process";
import { cp, rm } from "node:fs/promises";
import { createRequire } from "node:module";
import path from "node:path";
import { promisify } from "node:util";

type TraceOptions = {
  /** The packages the user listed in `app.server.traceDeps`. */
  traceDeps: string[];
  /** Whether `start-production` runs the migrations, which needs the Prisma CLI. */
  runsMigrations: boolean;
};

type TraceSelection = {
  /** Packages to trace even when nothing imports them. */
  traceInclude: string[];
  /** Packages to copy in full. */
  fullTraceInclude: string[];
};

/**
 * The Prisma CLI that `start-production` runs for migrations. Nothing imports
 * it, and it finds its engine binaries by path, so we copy both in full.
 *
 * TODO: Drop `@prisma/engines` once we upgrade to Prisma 8, whose CLI no
 * longer uses engine binaries.
 */
const prismaCliTraceDeps = ["prisma*", "@prisma/engines*"];

const serverDir = import.meta.dirname;
const outDir = path.join(serverDir, "production");
const requireFromServer = createRequire(path.join(serverDir, "package.json"));

await prepareProduction({
  traceDeps: [],
  runsMigrations: false,
});

async function prepareProduction({
  traceDeps,
  runsMigrations,
}: TraceOptions): Promise<void> {
  const { traceInclude, fullTraceInclude } = resolveTraceDeps([
    ...traceDeps,
    ...(runsMigrations ? prismaCliTraceDeps : []),
  ]);

  await rm(outDir, { recursive: true, force: true });
  const layout = await traceNodeModules(
    [
      path.join(serverDir, "bundle/server.js"),
      requireFromServer.resolve("dotenv/config"),
    ],
    {
      rootDir: serverDir,
      outDir,
      chmod: true,
      traceInclude,
      fullTraceInclude,
    },
  );
  await linkPackageBins();
  await cp(path.join(serverDir, "bundle"), path.join(outDir, "bundle"), {
    recursive: true,
  });

  console.log(
    `Traced ${Object.keys(layout.packages).length} packages into ${outDir}`,
  );
}

/**
 * Splits `app.server.traceDeps` into nf3 options, the same way Nitro does:
 * `pkg` traces the package, `pkg*` copies all its files, and `!pkg` removes a
 * package from nf3's built-in list of packages to copy in full.
 *
 * TODO: Remove once we migrate to Nitro, which parses `traceDeps` itself.
 *
 * @see https://nitro.build/config#tracedeps
 */
function resolveTraceDeps(traceDeps: string[]): TraceSelection {
  const excluded = new Set<string>();
  const traceInclude: string[] = [];
  const userFullTraceInclude: string[] = [];

  for (const selector of traceDeps) {
    if (selector.startsWith("!")) {
      excluded.add(selector.slice(1));
    } else if (selector.endsWith("*")) {
      const packageName = selector.slice(0, -1);
      traceInclude.push(packageName);
      userFullTraceInclude.push(packageName);
    } else {
      traceInclude.push(selector);
    }
  }

  const fullTraceInclude = [
    ...new Set([...FullTracePackages, ...userFullTraceInclude]),
  ].filter((packageName) => !excluded.has(packageName));

  return {
    // Traced as if imported: only the files they reach get copied.
    traceInclude: traceInclude.filter(
      (packageName) => !excluded.has(packageName),
    ),
    // Copied whole, for packages that load their own files by path.
    fullTraceInclude,
  };
}

/**
 * `production/` has no `node_modules/.bin`, because `npm install` never ran in
 * it. `npm rebuild` links the traced packages' bins, so production scripts can
 * call them (e.g. `prisma`).
 */
async function linkPackageBins(): Promise<void> {
  await promisify(execFile)("npm", ["rebuild", "--ignore-scripts"], {
    cwd: outDir,
  });
}
